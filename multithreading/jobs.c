#include <stdio.h>
#include <stdlib.h>

void cobol_job_handler_(int *id) {
    printf("Job #%d running from thread pool!\n", *id);
    sleep(1);
    printf("Job #%d complete.\n", *id);
}

void submit_job_(int *id) {
    int *arg = malloc(sizeof(int));
    *arg = *id;
    extern void push_job_(void (*job)(void*), void *arg);
    push_job_((void (*)(void*))cobol_job_handler_, arg);
}
