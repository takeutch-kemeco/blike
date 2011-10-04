CC = gcc
AR = ar

INCLUDE_BASE_PATH = ../../../include

SRCS =	drvalsa_player.c \
	drvalsa_player_thread.c

OBJS	= $(SRCS:.c=.o)
PROG	= ../../../libblike.a
DEFS	=
LIBS	= `pkg-config alsa --libs` \
	`pkg-config glib-2.0 --libs` \
	-lm
	
INCS	= -I $(INCLUDE_BASE_PATH) \
	`pkg-config glib-2.0 --cflags` \
	`pkg-config alsa --cflags`

OPTS	= -O2

all: $(PROG)
	ranlib $(PROG)

$(PROG): $(OBJS)
	$(AR) -rv $@ $(OBJS)

.c.o:
	$(CC) $(OPTS) $(INCS) $(LIBS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

