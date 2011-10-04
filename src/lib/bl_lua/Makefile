CC = gcc
AR = ar

INCLUDE_BASE_PATH = ../../../include

SRCS	= bllua_system.c \
	bllua_regist_bld_func.c \
	bllua_regist_bl_func.c

OBJS	= $(SRCS:.c=.o)
PROG	= ../../../libblike.a
DEFS	=
LIBS	= `pkg-config lua5.1 --libs`
INCS	= `pkg-config lua5.1 --cflags` \
	-I $(INCLUDE_BASE_PATH) \
	-I $(INCLUDE_BASE_PATH)/lib/bl_lua

OPTS	= -O2

all: $(PROG)
	ranlib $(PROG)

$(PROG): $(OBJS)
	$(AR) -rv $@ $(OBJS)

.c.o:
	$(CC) $(OPTS) $(DEFS) $(INCS) $(LIBS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

