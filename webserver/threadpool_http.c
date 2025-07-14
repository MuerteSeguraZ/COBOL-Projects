#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <netinet/in.h>
#include <pthread.h>
#include <signal.h>
#include <errno.h>
#include <stdint.h>
#include <ctype.h>

#define PORT 8080
#define THREADS 4
#define BUFFER_SIZE 16384   // Bigger buffer for full requests
#define RESPONSE_SIZE 8192

extern void cob_init(void);

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

// Helper: case-insensitive strstr
char *strcasestr(const char *haystack, const char *needle) {
    if (!*needle)
        return (char *)haystack;
    for (; *haystack; haystack++) {
        if (tolower((unsigned char)*haystack) == tolower((unsigned char)*needle)) {
            const char *h, *n;
            for (h = haystack, n = needle; *h && *n; h++, n++) {
                if (tolower((unsigned char)*h) != tolower((unsigned char)*n))
                    break;
            }
            if (!*n)
                return (char *)haystack;
        }
    }
    return NULL;
}

// Parse Content-Length header from HTTP headers
int parse_content_length(const char *headers) {
    const char *cl_str = "Content-Length:";
    char *p = strcasestr(headers, cl_str);
    if (!p) return 0; // no Content-Length header found
    p += strlen(cl_str);

    // skip whitespace
    while (*p && isspace((unsigned char)*p)) p++;

    int content_length = 0;
    sscanf(p, "%d", &content_length);
    return content_length > 0 ? content_length : 0;
}

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
        int total_read = 0;
        int content_length = 0;
        int headers_received = 0;

        while (1) {
            if (total_read >= (int)(sizeof(buffer) - 1)) {
                fprintf(stderr, "Request too large, rejecting\n");
                close(job.client_fd);
                goto next_job;
            }

            int n = read(job.client_fd, buffer + total_read, sizeof(buffer) - total_read - 1);
            if (n <= 0) {
                close(job.client_fd);
                goto next_job;
            }
            total_read += n;
            buffer[total_read] = '\0';

            if (!headers_received) {
                char *headers_end = strstr(buffer, "\r\n\r\n");
                if (headers_end) {
                    headers_received = 1;
                    content_length = parse_content_length(buffer);
                    int header_len = (int)(headers_end - buffer) + 4;

                    // If no body or full body read, break
                    if (content_length == 0 || total_read >= header_len + content_length) {
                        break;
                    }
                }
            } else {
                char *headers_end = strstr(buffer, "\r\n\r\n");
                int header_len = (int)(headers_end - buffer) + 4;
                if (total_read >= header_len + content_length) {
                    break;
                }
            }
        }

        int req_len = total_read;
        int32_t resp_len = 0;
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
                        break;
                    } else {
                        perror("write failed");
                        break;
                    }
                }
                total_written += written;
            }
        }

        close(job.client_fd);

    next_job:
        ; // continue loop
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
