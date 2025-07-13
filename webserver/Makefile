CC      = gcc
COBC    = cobc
CFLAGS  = -pthread -O2
COBFLAGS = -free -O2
TARGET  = webserver
COBFILES = http_handler.cob
CFILES   = threadpool_http.c
COB_OBJS = $(COBFILES:.cob=.o)
C_OBJS   = $(CFILES:.c=.o)

.PHONY: all clean

all: $(TARGET)

%.o: %.cob
	$(COBC) $(COBFLAGS) -c $< -o $@

%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

$(TARGET): $(COB_OBJS) $(C_OBJS)
	$(COBC) -x $(COBFLAGS) $(COB_OBJS) $(C_OBJS) -o $(TARGET)

clean:
	rm -f $(COB_OBJS) $(C_OBJS) $(TARGET)
