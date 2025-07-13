all: floppyraid

floppy_io.o: floppy_io.c
	gcc -c -fPIC floppy_io.c -o floppy_io.o

floppyraid: floppy_io.o floppyraid.cob
	cobc -x floppyraid.cob floppy_io.o -o floppyraid

clean:
	rm -f floppyraid floppy_io.o diagnosticsmode

