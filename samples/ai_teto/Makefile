CC = gcc

CFLAGS= -O3 -march=native -mtune=native -g `pkg-config blike --cflags`
LIBS = `pkg-config blike --libs`

SRCS = main.c IO_BLIKE.c PAD.c BLOCK.c PANEL.c MINO.c GEN_MINO.c AI.c TETO.c
OBJS = $(SRCS:.c=.o)
PROG = ai_teto

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LIBS)

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)
