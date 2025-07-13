# Project Name

This project demonstrates a blend of COBOL and C, including:

- **floppy**: Floppy disk RAID 5, diagnostics mode.
- **Multithreading**: Threadpool implementation mixing COBOL and C.
- **Webserver**: HTTP server handling requests (Only GET for now) via COBOL handlers with multithreading in C.

## Build & Run

Each folder has its own Makefile. Build and run instructions per folder:

```bash
cd COBOL && make
cd ../Multithreading && make
cd ../webserver && make
