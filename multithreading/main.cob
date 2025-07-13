       IDENTIFICATION DIVISION.
       PROGRAM-ID. COBOL-THREADS.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 NUM-THREADS      PIC S9(4) COMP-5 VALUE 4.
       77 JOB-ID           PIC S9(4) COMP-5.

       PROCEDURE DIVISION.
           DISPLAY "Initializing thread pool..."
           CALL "init_thread_pool" USING BY VALUE NUM-THREADS

           PERFORM VARYING JOB-ID FROM 1 BY 1 UNTIL JOB-ID > 10
               CALL "submit_job" USING BY VALUE JOB-ID
           END-PERFORM

           DISPLAY "Waiting for all jobs to complete..."
           CALL "shutdown_pool"

           DISPLAY "All threads terminated. Goodbye."
           STOP RUN.
