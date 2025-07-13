#include <stdint.h>
#include <stddef.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <sys/stat.h> // for mkdir
#include <errno.h>

#define FLOPPY_A "/dev/loop7"
#define FLOPPY_B "/dev/loop8"
#define FLOPPY_C "/dev/loop9"
#define NUM_DISKS 3
#define SECTOR_SIZE 512
#define DATA_SIZE (SECTOR_SIZE - sizeof(uint32_t))

const char *floppies[NUM_DISKS] = {FLOPPY_A, FLOPPY_B, FLOPPY_C};

void log_sector_error(const char *message, int sector) {
    mkdir("report", 0755);  // Ensure report/ exists

    FILE *log_file = fopen("report/raid5_log.txt", "a");
    if (!log_file) {
        perror("Failed to open raid5_log.txt");
        return;
    }

    time_t now = time(NULL);
    struct tm *t = localtime(&now);

    char time_str[64];
    strftime(time_str, sizeof(time_str), "%Y-%m-%d %H:%M:%S", t);

    fprintf(log_file, "[%s] Sector %d: %s\n", time_str, sector, message);
    fclose(log_file);
}

uint32_t xor_checksum(const uint8_t *data, size_t len) {
    uint32_t checksum = 0;
    for (size_t i = 0; i < len; i++) {
        checksum ^= data[i];
    }
    return checksum;
}

void xor_buffers(uint8_t *dest, uint8_t **buffers, int num_buffers, size_t len) {
    memset(dest, 0, len);
    for (int i = 0; i < num_buffers; i++) {
        for (size_t j = 0; j < len; j++) {
            dest[j] ^= buffers[i][j];
        }
    }
}

void write_raid5_floppy(const char *data, int *sector, int *parity_disk, int *num_disks) {
    int sector_val = *sector;
    int parity_val = *parity_disk;
    int num_disks_val = *num_disks;

    if (sector_val < 0) {
        fprintf(stderr, "Invalid sector number %d\n", sector_val);
        log_sector_error("Invalid negative sector", sector_val);
        return;
    }
    if (num_disks_val != NUM_DISKS) {
        fprintf(stderr, "Warning: num_disks (%d) does not match expected NUM_DISKS (%d)\n", num_disks_val, NUM_DISKS);
        // You can decide to accept or reject here
    }
    int fd[NUM_DISKS];
    off_t offset = (off_t)(sector_val / (num_disks_val - 1)) * SECTOR_SIZE;

    // Open floppy devices for writing
    for (int i = 0; i < NUM_DISKS; i++) {
        fd[i] = open(floppies[i], O_RDWR);
        if (fd[i] < 0) {
            fprintf(stderr, "Failed to open %s for writing: %s\n", floppies[i], strerror(errno));
            for (int j = 0; j < i; j++) close(fd[j]);
            return;
        }
    }

    uint8_t data_blocks[NUM_DISKS - 1][DATA_SIZE];
    uint8_t *data_ptrs[NUM_DISKS - 1];
    for (int i = 0; i < NUM_DISKS - 1; i++) {
        memcpy(data_blocks[i], data + i * DATA_SIZE, DATA_SIZE);
        data_ptrs[i] = data_blocks[i];
    }

    uint8_t parity_block[DATA_SIZE];
    xor_buffers(parity_block, data_ptrs, NUM_DISKS - 1, DATA_SIZE);

    for (int i = 0, data_index = 0; i < NUM_DISKS; i++) {
        uint8_t buffer[SECTOR_SIZE];
        if (i == parity_val) {
            memcpy(buffer, parity_block, DATA_SIZE);
        } else {
            memcpy(buffer, data_blocks[data_index++], DATA_SIZE);
        }

        uint32_t cs = xor_checksum(buffer, DATA_SIZE);
        memcpy(buffer + DATA_SIZE, &cs, sizeof(cs));

        if (lseek(fd[i], offset, SEEK_SET) < 0) {
            fprintf(stderr, "Failed to seek sector %ld on %s: %s\n", (long)offset, floppies[i], strerror(errno));
        }
        else if (write(fd[i], buffer, SECTOR_SIZE) != SECTOR_SIZE) {
            fprintf(stderr, "Failed to write sector %ld on %s: %s\n", (long)offset, floppies[i], strerror(errno));
        }
    }

    for (int i = 0; i < NUM_DISKS; i++) close(fd[i]);
}

void read_raid5_floppy(char *data, int *sector, int *parity_disk, int *num_disks) {
    int sector_val = *sector;
    int parity_val = *parity_disk;
    int num_disks_val = *num_disks;

    if (sector_val < 0) {
        fprintf(stderr, "Invalid sector number %d\n", sector_val);
        log_sector_error("Invalid negative sector", sector_val);
        return;
    }
    if (num_disks_val != NUM_DISKS) {
        fprintf(stderr, "Warning: num_disks (%d) does not match expected NUM_DISKS (%d)\n", num_disks_val, NUM_DISKS);
        // Accept or handle accordingly
    }
    int fd[NUM_DISKS];
    off_t offset = (off_t)(sector_val / (num_disks_val - 1)) * SECTOR_SIZE;

    uint8_t buffers[NUM_DISKS][SECTOR_SIZE];
    int valid[NUM_DISKS] = {0};

    for (int i = 0; i < NUM_DISKS; i++) {
        fd[i] = open(floppies[i], O_RDONLY);
        if (fd[i] < 0) {
            fprintf(stderr, "Failed to open %s for reading: %s\n", floppies[i], strerror(errno));
            continue;
        }

        if (lseek(fd[i], offset, SEEK_SET) < 0) {
            fprintf(stderr, "Failed to seek sector %ld on %s: %s\n", (long)offset, floppies[i], strerror(errno));
        }
        else if (read(fd[i], buffers[i], SECTOR_SIZE) != SECTOR_SIZE) {
            fprintf(stderr, "Failed to read sector %ld on %s: %s\n", (long)offset, floppies[i], strerror(errno));
        }
        else {
            uint32_t stored_cs;
            memcpy(&stored_cs, buffers[i] + DATA_SIZE, sizeof(stored_cs));
            uint32_t calc_cs = xor_checksum(buffers[i], DATA_SIZE);
            if (stored_cs == calc_cs) {
                valid[i] = 1;
            } else {
                fprintf(stderr, "Checksum mismatch floppy %d sector %d\n", i, sector_val);
                log_sector_error("Checksum mismatch", sector_val);
            }
        }

        if (fd[i] >= 0) close(fd[i]);
    }

    int valid_count = 0;
    for (int i = 0; i < NUM_DISKS; i++) if (valid[i]) valid_count++;

    if (valid_count < NUM_DISKS - 1) {
        fprintf(stderr, "Too many errors to recover sector %d\n", sector_val);
        log_sector_error("Too many errors to recover", sector_val);
        memset(data, 0, (NUM_DISKS - 1) * DATA_SIZE);
        return;
    }

    int missing = -1;
    for (int i = 0; i < NUM_DISKS; i++) {
        if (!valid[i]) {
            missing = i;
            break;
        }
    }

    if (missing >= 0) {
        uint8_t *xor_ptrs[NUM_DISKS - 1];
        int idx = 0;
        for (int i = 0; i < NUM_DISKS; i++) {
            if (i != missing) xor_ptrs[idx++] = buffers[i];
        }
        xor_buffers(buffers[missing], xor_ptrs, NUM_DISKS - 1, SECTOR_SIZE);
        valid[missing] = 1;
        log_sector_error("Recovered missing disk data via parity", sector_val);
    }

    int data_index = 0;
    for (int i = 0; i < NUM_DISKS; i++) {
        if (i == parity_val) continue;
        memcpy(data + data_index * DATA_SIZE, buffers[i], DATA_SIZE);
        data_index++;
    }
}
