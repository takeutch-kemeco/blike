CC = gcc
AR = ar

SRCS =	drvgtk_main.c \
	drvgtk_interface.c \
	drvgtk_system.c \
	drvgtk_signal_chain.c \
	drvgtk_signal.c \
	drvgtk_pthread.c \
	drvgtk_key_ring_buffer.c \
	drvgtk_transrate_keycode.c \
	main_window.c \
	main_screen.c \
	create_xpm.c \
	hankaku_font.c

OBJS	= $(SRCS:.c=.o)
SONAME	= libblike-gtk.a.0.1
PROG	= $(SONAME).5

LIBS	= `pkg-config gtk+-2.0 --libs` -lpthread
INCS	= -I../../ -I../common `pkg-config gtk+-2.0 --cflags`

OPTS 	=
#OPTS	= -O4 -march=core2 -msse3

PREFIX		= /usr/local
PREFIX_LIB	= $(PREFIX)/lib
PREFIX_INC	= $(PREFIX)/include

PREFIX_INST_LIB	= $(PREFIX_LIB)/blike
PREFIX_INST_INC	= $(PREFIX_INC)/blike



all: $(PROG)
	ranlib $(PROG)

$(PROG): $(OBJS)
	$(AR) $(OPTS) -rv $@ $(OBJS)

.c.o:
	$(CC) $(OPTS) $(INCS) $(LIBS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

install:
	mkdir -p $(PREFIX_INST_LIB)
	cp -f $(PROG) $(PREFIX_INST_LIB)
	ldconfig $(PREFIX_INST_LIB)
	rm -f $(PREFIX_INST_LIB)/$(SONAME)
	ln -s $(PREFIX_INST_LIB)/$(PROG) $(PREFIX_INST_LIB)/$(SONAME)

#uninstall:
#	rm $(BIN)/$(PROG)
