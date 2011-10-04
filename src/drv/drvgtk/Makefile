CC = gcc
AR = ar

INCLUDE_BASE_PATH = ../../../include

SRCS =	drvgtk_main.c \
	drvgtk_interface.c \
	drvgtk_system.c \
	drvgtk_signal_chain.c \
	drvgtk_signal.c \
	drvgtk_pthread.c \
	drvgtk_key_ring_buffer.c \
	drvgtk_transrate_keycode.c \
	drvgtk_keybord_state.c \
	drvgtk_sleep.c \
	drvgtk_malloc.c \
	main_window.c \
	main_screen.c \
	hankaku_font.c

OBJS	= $(SRCS:.c=.o)
PROG	= ../../../libblike.a
DEFS	=
LIBS	= `pkg-config gtk+-3.0 --libs` \
	-lm
	
INCS	= -I $(INCLUDE_BASE_PATH) \
	-I $(INCLUDE_BASE_PATH)/common \
	`pkg-config gtk+-3.0 --cflags`

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

