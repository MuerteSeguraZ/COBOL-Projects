#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdint.h>
#include <time.h>

#define DRIVE0 "/dev/loop7"
#define DRIVE1 "/dev/loop8"
#define DRIVE2 "/dev/loop9"
#define SECTOR_SIZE 512
#define DATA_SIZE (SECTOR_SIZE - sizeof(uint32_t))

const char *drives[3] = { DRIVE0, DRIVE1, DRIVE2 };

uint32_t xor_checksum(const char *data, size_t len) {
    uint32_t cs = 0;
    for (size_t i = 0; i < len; i++) {
        cs ^= (uint8_t)data[i];
    }
    return cs;
}

void log_sector_error(const char *message, int sector) {
    FILE *log_file = fopen("raid5_log.txt", "a");
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

/**
 * Reads a sector from RAID5 array of 3 drives.
 * sector: pointer to sector index.
 * data: buffer to store DATA_SIZE bytes of data.
 * status:
 *    0 = valid,
 *    1 = recovered (one drive corrupted, data reconstructed),
 *    2 = corrupted (more than one drive corrupted or unrecoverable).
 */
void read_raid5_sector(char *data, int *sector, int *status) {
    int fds[3];
    uint8_t buffers[3][SECTOR_SIZE];
    int valid[3] = {0, 0, 0};

    off_t offset = (off_t)(*sector) * SECTOR_SIZE;

    for (int i = 0; i < 3; i++) {
        fds[i] = open(drives[i], O_RDONLY);
        if (fds[i] < 0) {
            perror("open");
            valid[i] = 0;
        }
    }

    int bad_count = 0;
    for (int i = 0; i < 3; i++) {
        if (fds[i] < 0) {
            bad_count++;
            continue;
        }

        if (lseek(fds[i], offset, SEEK_SET) < 0 ||
            read(fds[i], buffers[i], SECTOR_SIZE) != SECTOR_SIZE) {
            perror("read");
            bad_count++;
            valid[i] = 0;
            close(fds[i]);
            fds[i] = -1;
            continue;
        }

        uint32_t stored_cs;
        memcpy(&stored_cs, buffers[i] + DATA_SIZE, sizeof(uint32_t));
        uint32_t calc_cs = xor_checksum((char*)buffers[i], DATA_SIZE);

        if (stored_cs == calc_cs) {
            valid[i] = 1;
        } else {
            fprintf(stderr, "Checksum mismatch drive %d sector %d\n", i, *sector);
            char logmsg[128];
            snprintf(logmsg, sizeof(logmsg), "Checksum mismatch on drive %d", i);
            log_sector_error(logmsg, *sector);
            valid[i] = 0;
            bad_count++;
        }

        close(fds[i]);
        fds[i] = -1;
    }

    if (bad_count > 1) {
        fprintf(stderr, "Too many corrupted drives at sector %d\n", *sector);
        log_sector_error("Too many corrupted drives, unrecoverable", *sector);
        memset(data, ' ', DATA_SIZE);
        *status = 2;
        return;
    }

    if (bad_count == 0) {
        int parity_drive = (*sector) % 3;

        int data_drive1 = (parity_drive + 1) % 3;
        int data_drive2 = (parity_drive + 2) % 3;

        uint8_t parity_check[DATA_SIZE];
        for (int i = 0; i < DATA_SIZE; i++) {
            parity_check[i] = buffers[data_drive1][i] ^ buffers[data_drive2][i];
        }

        if (memcmp(parity_check, buffers[parity_drive], DATA_SIZE) != 0) {
            fprintf(stderr, "Parity mismatch at sector %d\n", *sector);
            log_sector_error("Parity mismatch detected", *sector);
            memcpy(data, buffers[data_drive1], DATA_SIZE);
            *status = 1; 
            return;
        }

        memcpy(data, buffers[data_drive1], DATA_SIZE);
        *status = 0; 
        return;
    }

    int corrupted_drive = -1;
    for (int i = 0; i < 3; i++) {
        if (!valid[i]) {
            corrupted_drive = i;
            break;
        }
    }

    int valid_drive1 = (corrupted_drive + 1) % 3;
    int valid_drive2 = (corrupted_drive + 2) % 3;

    for (int i = 0; i < DATA_SIZE; i++) {
        buffers[corrupted_drive][i] = buffers[valid_drive1][i] ^ buffers[valid_drive2][i];
    }

    int parity_drive = (*sector) % 3;
    if (corrupted_drive == parity_drive) {
        int data_drive = (parity_drive + 1) % 3;
        memcpy(data, buffers[data_drive], DATA_SIZE);
    } else {
        memcpy(data, buffers[corrupted_drive], DATA_SIZE);
    }

    char logmsg[128];
    snprintf(logmsg, sizeof(logmsg), "Drive %d corrupted, data recovered by parity", corrupted_drive);
    log_sector_error(logmsg, *sector);

    *status = 1; 
}

int main() {
    int sector = 0;
    char data[DATA_SIZE];
    int status = 0;

    read_raid5_sector(data, &sector, &status);

    printf("Read sector %d status: %d\n", sector, status);
    printf("Data (first 32 bytes):\n");
    for (int i = 0; i < 32 && i < DATA_SIZE; i++) {
        printf("%02X ", (unsigned char)data[i]);
    }
    printf("\n");

    return 0;
}
