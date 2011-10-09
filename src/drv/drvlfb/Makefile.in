CC = gcc

BASE_PATH = ../../..
INCLUDE_BASE_PATH = $(BASE_PATH)/include

SRCS =  drvlfb_main.c \
        drvlfb_interface.c \
        drvlfb_system.c \
        drvlfb_sleep.c \
        drvlfb_malloc.c \
        hankaku_font.c

OBJS	= $(SRCS:.c=.o)
SONAME	= libbl_drvlfb.so
PROG	= $(SONAME)
DEFS	=
LIBS	= -lpthread \
	-lm

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

