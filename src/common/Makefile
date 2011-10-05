CC = gcc

BASE_PATH = ../..
INCLUDE_BASE_PATH = $(BASE_PATH)/include

COMMON_SRCS = char8b.c \
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
	bl_malloc_aligned.c \
	bl_draw_2db_spline.c \
	test.c

OBJS	= $(COMMON_SRCS:.c=.o)
SONAME	= libbl_common.so
PROG	= $(SONAME)
DEFS	=
LIBS	=
INCS	= -I $(INCLUDE_BASE_PATH) \
	-I $(INCLUDE_BASE_PATH)/common

OPTS	= -O2

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) -shared -Wl,-soname,$(SONAME) -o $@ $(OBJS) $(LIBS)

.c.o:
	$(CC) -fPIC $(OPTS) $(DEFS) $(INCS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)
