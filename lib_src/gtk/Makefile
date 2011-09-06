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
	drvgtk_keybord_state.c \
	drvgtk_sleep.c \
	drvgtk_malloc.c \
	main_window.c \
	main_screen.c \
	hankaku_font.c

COMMON_PATH = ../common
COMMON_SRCS = $(COMMON_PATH)/char8b.c \
	$(COMMON_PATH)/clock.c \
	$(COMMON_PATH)/cls.c \
	$(COMMON_PATH)/color.c \
	$(COMMON_PATH)/drawline.c \
	$(COMMON_PATH)/drawrect.c \
	$(COMMON_PATH)/fillrect.c \
	$(COMMON_PATH)/flshwin.c \
	$(COMMON_PATH)/getpix.c \
	$(COMMON_PATH)/icol.c \
	$(COMMON_PATH)/initfont.c \
	$(COMMON_PATH)/inptint.c \
	$(COMMON_PATH)/inptflot.c \
	$(COMMON_PATH)/locate.c \
	$(COMMON_PATH)/openwin.c \
	$(COMMON_PATH)/printf.c \
	$(COMMON_PATH)/putc.c \
	$(COMMON_PATH)/puts.c \
	$(COMMON_PATH)/puts1.c \
	$(COMMON_PATH)/rand.c \
	$(COMMON_PATH)/rgb.c \
	$(COMMON_PATH)/rnd.c \
	$(COMMON_PATH)/scanf.c \
	$(COMMON_PATH)/setcol.c \
	$(COMMON_PATH)/setbcol.c \
	$(COMMON_PATH)/setmode.c \
	$(COMMON_PATH)/setpix.c \
	$(COMMON_PATH)/srand.c \
	$(COMMON_PATH)/main.c \
	$(COMMON_PATH)/vscanf.c \
	$(COMMON_PATH)/wait.c \
	$(COMMON_PATH)/waitnf.c \
	$(COMMON_PATH)/keybuf.c \
	$(COMMON_PATH)/inkey.c \
	$(COMMON_PATH)/gets.c \
	$(COMMON_PATH)/filloval.c \
	$(COMMON_PATH)/drawstr.c \
	$(COMMON_PATH)/slctwin.c \
	$(COMMON_PATH)/openvwin.c \
	$(COMMON_PATH)/copyrct0.c \
	$(COMMON_PATH)/copyrct1.c \
	$(COMMON_PATH)/readywin.c \
	$(COMMON_PATH)/setptn0.c \
	$(COMMON_PATH)/drwptn_e.c \
	$(COMMON_PATH)/drwptn_r.c \
	$(COMMON_PATH)/drwptn_d.c \
	$(COMMON_PATH)/bl_malloc_aligned.c \
	$(COMMON_PATH)/bl_draw_2db_spline.c \
	$(COMMON_PATH)/test.c

BL3D_PATH = ../bl3d
BL3D_SRCS = $(BL3D_PATH)/bl3d_system.c \
	$(BL3D_PATH)/bl3d_matrix.c \
	$(BL3D_PATH)/bl3d_math.c \
	$(BL3D_PATH)/bl3d_dobj.c \
	$(BL3D_PATH)/bl3d_packet_pool.c \
	$(BL3D_PATH)/bl3d_frame_buffer.c \
	$(BL3D_PATH)/bl3d_triangle.c

BL_LOAD_XPM_PATH = ../bl_load_xpm
BL_LOAD_XPM_SRCS = $(BL_LOAD_XPM_PATH)/bl_load_xpm.c

BL_PLOT_PATH = ../bl_plot
BL_PLOT_SRCS = $(BL_PLOT_PATH)/bl_plot.c

OBJS	= $(COMMON_SRCS:.c=.o) $(SRCS:.c=.o) $(BL_LOAD_XPM_SRCS:.c=.o) $(BL3D_SRCS:.c=.o) $(BL_PLOT_SRCS:.c=.o)
SONAME	= libblike-gtk.a.0.1
PROG	= $(SONAME).5

LIBS	= `pkg-config gtk+-3.0 --libs` -lm
INCS	= -I../../ -I../common -I../bl_load_xpm -I../bl_plot -I../bl3d `pkg-config gtk+-3.0 --cflags`

OPTS	= -O2 -g -march=core2 -msse3

PREFIX		= /usr/local
PREFIX_LIB	= $(PREFIX)/lib
PREFIX_INC	= $(PREFIX)/include

PREFIX_INST_LIB	= $(PREFIX_LIB)/blike
PREFIX_INST_INC	= $(PREFIX_INC)/blike



all: $(PROG)
	ranlib $(PROG)

$(PROG): $(OBJS)
	$(AR) -rv $@ $(OBJS)

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
