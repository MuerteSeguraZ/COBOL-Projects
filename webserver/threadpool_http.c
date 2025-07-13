#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>
#include <stdint.h>

#define PORT 8080
#define THREADS 4
#define BUFFER_SIZE 4096
#define RESPONSE_SIZE 8192

extern void cob_init(void);

// COBOL handler takes pointers to buffers and lengths
extern void http_handler(char *request_data, int *request_len, char *response_data, int32_t *response_len);

typedef struct {
    int client_fd;
} job_t;

pthread_t threads[THREADS];
pthread_mutex_t job_mutex = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t job_cond = PTHREAD_COND_INITIALIZER;
job_t job_queue[100];
int job_count = 0;
int job_index = 0;

void *worker(void *arg) {
    while (1) {
        pthread_mutex_lock(&job_mutex);
        while (job_count == 0) {
            pthread_cond_wait(&job_cond, &job_mutex);
        }
        job_t job = job_queue[job_index];
        job_index = (job_index + 1) % 100;
        job_count--;
        pthread_mutex_unlock(&job_mutex);

        char buffer[BUFFER_SIZE];
        int bytes_read = read(job.client_fd, buffer, BUFFER_SIZE - 1);
        if (bytes_read <= 0) {
            close(job.client_fd);
            continue;
        }
        buffer[bytes_read] = '\0';

        int req_len = bytes_read;
        int32_t resp_len = 0; // Use int32_t explicitly for compatibility
        char response[RESPONSE_SIZE];
        memset(response, 0, sizeof(response));

        printf("Before COBOL call: req_len=%d, resp_len=%d\n", req_len, resp_len);
        http_handler(buffer, &req_len, response, &resp_len);

        unsigned char *p = (unsigned char *)&resp_len;
        printf("Raw bytes of resp_len: %02X %02X %02X %02X\n", p[0], p[1], p[2], p[3]);

        printf("After COBOL call: req_len=%d, resp_len=%d\n", req_len, resp_len);

        if (resp_len <= 0 || resp_len > RESPONSE_SIZE) {
            fprintf(stderr, "Invalid COBOL response length %d, ignoring response\n", resp_len);
            resp_len = 0;
        }

        printf("COBOL response length: %d\n", resp_len);
        printf("COBOL response preview:\n%.*s\n", resp_len < 512 ? resp_len : 512, response);

        if (resp_len > 0) {
            ssize_t total_written = 0;
            while (total_written < resp_len) {
                ssize_t written = write(job.client_fd, response + total_written, resp_len - total_written);
                if (written <= 0) {
                    if (errno == EPIPE) {
                        break; // client closed connection
                    } else {
                        perror("write failed");
                        break;
                    }
                }
                total_written += written;
            }
        }

        close(job.client_fd);
    }
    return NULL;
}

void submit_job(int client_fd) {
    pthread_mutex_lock(&job_mutex);
    int next_pos = (job_index + job_count) % 100;
    job_queue[next_pos].client_fd = client_fd;
    job_count++;
    pthread_cond_signal(&job_cond);
    pthread_mutex_unlock(&job_mutex);
}

int main() {
    signal(SIGPIPE, SIG_IGN);

    cob_init();

    int server_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (server_fd < 0) {
        perror("socket failed");
        exit(EXIT_FAILURE);
    }

    struct sockaddr_in addr = {0};
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = INADDR_ANY;
    addr.sin_port = htons(PORT);

    if (bind(server_fd, (struct sockaddr*)&addr, sizeof(addr)) < 0) {
        perror("bind failed");
        close(server_fd);
        exit(EXIT_FAILURE);
    }

    if (listen(server_fd, 10) < 0) {
        perror("listen failed");
        close(server_fd);
        exit(EXIT_FAILURE);
    }

    for (int i = 0; i < THREADS; i++) {
        if (pthread_create(&threads[i], NULL, worker, NULL) != 0) {
            perror("pthread_create failed");
            close(server_fd);
            exit(EXIT_FAILURE);
        }
    }

    printf("Server listening on port %d\n", PORT);

    while (1) {
        int client_fd = accept(server_fd, NULL, NULL);
        if (client_fd < 0) {
            perror("accept failed");
            continue;
        }
        submit_job(client_fd);
    }

    return 0;
}
