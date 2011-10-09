CC = gcc

BASE_PATH = ../../..
INCLUDE_BASE_PATH = $(BASE_PATH)/include

SRCS	= bllua_system.c \
	bllua_regist_bld_func.c \
	bllua_regist_bl_func.c

OBJS	= $(SRCS:.c=.o)
SONAME	= libbl_lua.so
PROG	= $(SONAME)
DEFS	=
LIBS	= `pkg-config lua5.1 --libs`
INCS	= `pkg-config lua5.1 --cflags` \
	-I $(INCLUDE_BASE_PATH) \
	-I $(INCLUDE_BASE_PATH)/lib/bl_lua

OPTS	= -O2

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) -shared -Wl,-soname,$(SONAME) -o $@ $(OBJS) $(LIBS)

.c.o:
	$(CC) -fPIC $(OPTS) $(DEFS) $(INCS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

