# Makefile

COBOL=cobc
CC=gcc
CFLAGS=-Wall -pthread
COBOLFLAGS=-x -free

TARGET=threadpool_app
COBOL_SRC=main.cob
C_SRC=threadpool.c

all: $(TARGET)

$(TARGET): $(COBOL_SRC) $(C_SRC)
	$(COBOL) $(COBOLFLAGS) $(COBOL_SRC) $(C_SRC) -lpthread -o $(TARGET)

clean:
	rm -f $(TARGET) *.o *.lst *.map *.cob~ *.so
