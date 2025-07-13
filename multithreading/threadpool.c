// threadpool.c
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>

#define MAX_JOBS 100

typedef struct {
    int job_id;
} job_t;

typedef struct {
    pthread_mutex_t mutex;
    pthread_cond_t cond;
    job_t jobs[MAX_JOBS];
    int job_count;
    int job_index;
    int shutdown;
    pthread_t *threads;
    int num_threads;
} thread_pool_t;

thread_pool_t pool;

void* worker(void* arg) {
    while (1) {
        pthread_mutex_lock(&pool.mutex);

        while (pool.job_count == 0 && !pool.shutdown) {
            pthread_cond_wait(&pool.cond, &pool.mutex);
        }

        if (pool.shutdown && pool.job_count == 0) {
            pthread_mutex_unlock(&pool.mutex);
            break;
        }

        // Get job
        job_t job = pool.jobs[pool.job_index];
        pool.job_index = (pool.job_index + 1) % MAX_JOBS;
        pool.job_count--;

        pthread_mutex_unlock(&pool.mutex);

        // Do the work: simple example
        printf("[Thread %lu] Processing job ID: %d\n", pthread_self(), job.job_id);
        sleep(1);  // simulate work
    }
    return NULL;
}

void init_thread_pool(int num_threads) {
    pool.num_threads = num_threads;
    pool.job_count = 0;
    pool.job_index = 0;
    pool.shutdown = 0;
    pthread_mutex_init(&pool.mutex, NULL);
    pthread_cond_init(&pool.cond, NULL);
    pool.threads = malloc(sizeof(pthread_t) * num_threads);

    for (int i = 0; i < num_threads; i++) {
        pthread_create(&pool.threads[i], NULL, worker, NULL);
    }

    printf("Thread pool initialized with %d threads\n", num_threads);
}

void submit_job(int job_id) {
    pthread_mutex_lock(&pool.mutex);

    // Add job to queue (simple ring buffer)
    int next_pos = (pool.job_index + pool.job_count) % MAX_JOBS;
    pool.jobs[next_pos].job_id = job_id;
    pool.job_count++;

    pthread_cond_signal(&pool.cond);
    pthread_mutex_unlock(&pool.mutex);

    printf("Job %d submitted\n", job_id);
}

void shutdown_pool() {
    pthread_mutex_lock(&pool.mutex);
    pool.shutdown = 1;
    pthread_cond_broadcast(&pool.cond);
    pthread_mutex_unlock(&pool.mutex);

    for (int i = 0; i < pool.num_threads; i++) {
        pthread_join(pool.threads[i], NULL);
    }

    free(pool.threads);

    pthread_mutex_destroy(&pool.mutex);
    pthread_cond_destroy(&pool.cond);

    printf("Thread pool shutdown complete\n");
}
