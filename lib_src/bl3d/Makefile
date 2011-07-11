CC = gcc
DEFS =



SRCS = test_bl3d_matrix.c
OBJS = $(SRCS:.c=.o)
PROG = test_bl3d_matrix



LIBBLIKE = ../gtk/libblike-gtk.a.0.1.5

LIBS = `pkg-config gtk+-2.0 --libs`
INCS = `pkg-config gtk+-2.0 --cflags` -I../../ -I../bl_load_xpm

OPTS = -O0 -g -march=core2 -msse3 -pg

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
