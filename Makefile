CC = gcc
CP = cp
RM = rm
MK = make
MD = mkdir
LN = ln

INSTALL_PREFIX_PATH = /usr
INSTALL_PREFIX_LIB_PATH = $(INSTALL_PREFIX_PATH)/lib
INSTALL_PREFIX_INCLUDE_PATH = $(INSTALL_PREFIX_PATH)/include
INSTALL_PREFIX_PKGCONFIG_PATH = /usr/lib/pkgconfig
INSTALL_PC_NAME = c_blike.pc
INSTALL_DIR_NAME = c_blike

BASE_PATH = $(shell pwd)
PKGCONFIG_BASE_PATH = $(BASE_PATH)/pkgconfig
INCLUDE_BASE_PATH = $(BASE_PATH)/include

PRE_SONAME = libc_blike.so
SONAME = $(PRE_SONAME).0.1
PROG = $(SONAME).5.1
DEFS =
LIBS = `pkg-config gtk+-3.0 glib-2.0 lua5.1 alsa --libs` \
	-lm \
	-lpthread

INCS = `pkg-config gtk+-3.0 glib-2.0 lua5.1 alsa --cflags`
OPTS = -O2

WRAPPER_NAME	 = bl_main_wrapper.a
WRAPPER_PATH	 = ./src/drv/wrapper

COMMON_PATH	 = ./src/common
DRVGTK_PATH	 = ./src/drv/drvgtk
DRVALSA_PATH	 = ./src/drv/drvalsa
DRVLFB_PATH	 = ./src/drv/drvlfb
BL3D_PATH	 = ./src/lib/bl3d
BL_LOAD_XPM_PATH = ./src/lib/bl_load_xpm
BL_LUA_PATH	 = ./src/lib/bl_lua
BL_MATH_PATH	 = ./src/lib/bl_math
BL_PLOT_PATH	 = ./src/lib/bl_plot
BL_SPLINE_PATH	 = ./src/lib/bl_spline

OBJS =	$(COMMON_PATH)/*.o	\
	$(DRVGTK_PATH)/*.o	\
	$(DRVALSA_PATH)/*.o	\
	$(BL3D_PATH)/*.o	\
	$(BL_LOAD_XPM_PATH)/*.o	\
	$(BL_LUA_PATH)/*.o	\
	$(BL_MATH_PATH)/*.o	\
	$(BL_PLOT_PATH)/*.o	\
	$(BL_SPLINE_PATH)/*.o

all:
	$(MK) -C $(WRAPPER_PATH)	DEFS=$(DEFS)
	$(MK) -C $(COMMON_PATH)		DEFS=$(DEFS)
	$(MK) -C $(DRVGTK_PATH)		DEFS=$(DEFS)
	$(MK) -C $(DRVALSA_PATH)	DEFS=$(DEFS)
	$(MK) -C $(DRVLFB_PATH)		DEFS=$(DEFS)
	$(MK) -C $(BL3D_PATH)		DEFS=$(DEFS)
	$(MK) -C $(BL_LOAD_XPM_PATH)	DEFS=$(DEFS)
	$(MK) -C $(BL_LUA_PATH)		DEFS=$(DEFS)
	$(MK) -C $(BL_MATH_PATH)	DEFS=$(DEFS)
	$(MK) -C $(BL_PLOT_PATH)	DEFS=$(DEFS)
	$(MK) -C $(BL_SPLINE_PATH)	DEFS=$(DEFS)
	$(CC) -shared -Wl,-soname,$(SONAME) -o $(PROG) $(OBJS) $(LIBS)
	
clean:
	$(MK) -C $(WRAPPER_PATH)	clean
	$(MK) -C $(COMMON_PATH)		clean
	$(MK) -C $(DRVGTK_PATH)		clean
	$(MK) -C $(DRVALSA_PATH)	clean
	$(MK) -C $(DRVLFB_PATH)		clean
	$(MK) -C $(BL3D_PATH)		clean
	$(MK) -C $(BL_LOAD_XPM_PATH)	clean
	$(MK) -C $(BL_LUA_PATH)		clean
	$(MK) -C $(BL_MATH_PATH)	clean
	$(MK) -C $(BL_PLOT_PATH)	clean
	$(MK) -C $(BL_SPLINE_PATH)	clean
	$(RM) -f $(PROG)

install:
	$(MD) -p $(INSTALL_PREFIX_LIB_PATH)
	$(CP) -f $(WRAPPER_PATH)/$(WRAPPER_NAME) \
		$(INSTALL_PREFIX_LIB_PATH)
	$(CP) -f $(PROG) \
		$(INSTALL_PREFIX_LIB_PATH)
	/sbin/ldconfig
	$(LN) -nf $(INSTALL_PREFIX_LIB_PATH)/$(PROG) \
		$(INSTALL_PREFIX_LIB_PATH)/$(PRE_SONAME)
	$(MD) -p $(INSTALL_PREFIX_INCLUDE_PATH)/$(INSTALL_DIR_NAME)
	$(CP) -r $(INCLUDE_BASE_PATH)/* \
		$(INSTALL_PREFIX_INCLUDE_PATH)/$(INSTALL_DIR_NAME)
	$(CP) -f $(PKGCONFIG_BASE_PATH)/$(INSTALL_PC_NAME) \
		$(INSTALL_PREFIX_PKGCONFIG_PATH)

uninstall:
	$(RM) -f $(INSTALL_PREFIX_LIB_PATH)/$(PROG)
	$(RM) -f $(INSTALL_PREFIX_LIB_PATH)/$(PRE_SONAME)
	$(RM) -f $(INSTALL_PREFIX_LIB_PATH)/$(WRAPPER_NAME)
	$(RM) -r $(INSTALL_PREFIX_INCLUDE_PATH)/$(INSTALL_DIR_NAME)
	$(RM) $(INSTALL_PREFIX_PKGCONFIG_PATH)/$(INSTALL_PC_NAME)
