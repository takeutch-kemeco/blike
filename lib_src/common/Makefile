CC = gcc
AR = ar
SRCS =	char8b.c \
	clock.c \
	cls.c \
	color.c \
	drawline.c \
	drawrect.c \
	fillrect.c \
	flshwin.c \
	getpix.c \
	icol.c \
	initfont.c \
	inptint.c \
	inptflot.c \
	locate.c \
	openwin.c \
	printf.c \
	putc.c \
	puts.c \
	puts1.c \
	rand.c \
	rgb.c \
	rnd.c \
	scanf.c \
	setcol.c \
	setbcol.c \
	setmode.c \
	setpix.c \
	srand.c \
	main.c \
	vscanf.c \
	wait.c \
	waitnf.c \
	keybuf.c \
	inkey.c \
	gets.c \
	filloval.c \
	drawstr.c \
	slctwin.c \
	openvwin.c \
	copyrct0.c \
	copyrct1.c \
	readywin.c \
	setptn0.c \
	drwptn_e.c \
	drwptn_r.c \
	drwptn_d.c \
	test.c

OBJS	= $(SRCS:.c=.o)
SONAME	= libblike.a.0.1
PROG	= $(SONAME).5

LIBS	= -lpthread
INCS	= -I../../

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
