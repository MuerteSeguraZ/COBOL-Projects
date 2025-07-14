#ifndef THREADPOOL_H
#define THREADPOOL_H

void init_thread_pool(int num_threads);
void submit_job(int job_id);
void shutdown_pool(void);

#endif
