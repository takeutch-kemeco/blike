CC = gcc
RM = rm

SRCS =
OBJS = $(SRCS:.c=.o)
PROG =
DEFS =
LIBS = `pkg-config c_blike --libs`
INCS = `pkg-config c_blike --cflags`
OPTS = -O2

all:
	$(CC) $(OPTS) $(DEFS) $(INCS) -o $(PROG) $(SRCS) $(LIBS)

.c.o:
	$(CC) $(OPTS) $(INCS) $(DEFS) -c -o $@ $<

clean:
	$(RM) -f $(OBJS) $(PROG)

