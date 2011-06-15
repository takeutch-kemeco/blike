CC = gcc
DEFS =



SRCS =
OBJS = $(SRCS:.c=.o)
PROG =



LIBBLIKE = ../lib_src/gtk/libblike-gtk.a.0.1.5

LIBS = `pkg-config gtk+-2.0 --libs`
INCS = `pkg-config gtk+-2.0 --cflags` -I ../

OPTS =
#OPTS = -O4 -march=core2 -msse3

BIN = /usr/local/bin



all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(OPTS) -o $@ $(OBJS) $(LIBBLIKE) $(LIBS) $(INCS)

.c.o:
	$(CC) $(OPTS) $(INCS) $(DEFS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

#install:
#	cp $(PROG) $(BIN)

#uninstall:
#	rm $(BIN)/$(PROG)

