CC = gcc
AR = ar
CP = cp
RM = rm
MK = make
MD = mkdir

INSTALL_PREFIX_PATH = /usr
INSTALL_PREFIX_LIB_PATH = $(INSTALL_PREFIX_PATH)/lib
INSTALL_PREFIX_INCLUDE_PATH = $(INSTALL_PREFIX_PATH)/include
INSTALL_PREFIX_PKGCONFIG_PATH = $(INSTALL_PREFIX_PATH)/lib/pkgconfig
INSTALL_PC_NAME = c_blike.pc
INSTALL_DIR_NAME = c_blike

PKGCONFIG_BASE_PATH = ./pkgconfig
INCLUDE_BASE_PATH = ./include

SRCS =
OBJS =	./src/common/common.a \
	./src/drv/drvgtk/drvgtk.a \
	./src/drv/drvalsa/drvalsa.a \
	./src/lib/bl3d/bl3d.a \
	./src/lib/bl_load_xpm/bl_load_xpm.a \
	./src/lib/bl_lua/bl_lua.a \
	./src/lib/bl_math/bl_math.a \
	./src/lib/bl_plot/bl_plot.a

SONAME = libblike-gtk.a.0.1
PROG = $(SONAME).5
DEFS =
LIBS =
INCS =
OPTS = -O2

all:
	$(MK) -C ./src/common
	$(MK) -C ./src/drv/drvgtk
	$(MK) -C ./src/drv/drvalsa
	$(MK) -C ./src/drv/drvlfb
	$(MK) -C ./src/lib/bl3d
	$(MK) -C ./src/lib/bl_load_xpm
	$(MK) -C ./src/lib/bl_lua
	$(MK) -C ./src/lib/bl_math
	$(MK) -C ./src/lib/bl_plot

clean:
	$(MK) -C ./src/common clean
	$(MK) -C ./src/drv/drvgtk clean
	$(MK) -C ./src/drv/drvalsa clean
	$(MK) -C ./src/drv/drvlfb clean
	$(MK) -C ./src/lib/bl3d clean
	$(MK) -C ./src/lib/bl_load_xpm clean
	$(MK) -C ./src/lib/bl_lua clean
	$(MK) -C ./src/lib/bl_math clean
	$(MK) -C ./src/lib/bl_plot clean

install:
	$(MD) -p $(INSTALL_PREFIX_LIB_PATH)/$(INSTALL_DIR_NAME)
	$(CP) libblike.a $(INSTALL_PREFIX_LIB_PATH)/$(INSTALL_DIR_NAME)/$(PROG)
	$(MD) -p $(INSTALL_PREFIX_INCLUDE_PATH)/$(INSTALL_DIR_NAME)
	$(CP) -r $(INCLUDE_BASE_PATH)/* $(INSTALL_PREFIX_INCLUDE_PATH)/$(INSTALL_DIR_NAME)
	$(CP) $(PKGCONFIG_BASE_PATH)/$(INSTALL_PC_NAME) $(INSTALL_PREFIX_PKGCONFIG_PATH)

uninstall:
	$(RM) -r $(INSTALL_PREFIX_LIB_PATH)/$(INSTALL_DIR_NAME)
	$(RM) -r $(INSTALL_PREFIX_INCLUDE_PATH)/$(INSTALL_DIR_NAME)
	$(RM) $(INSTALL_PREFIX_PKGCONFIG_PATH)/$(INSTALL_PC_NAME)

