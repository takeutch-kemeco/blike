CC = gcc
AR = ar

INCLUDE_BASE_PATH = ../../../include

SRCS =  drvlfb_main.c \
        drvlfb_interface.c \
        drvlfb_system.c \
        drvlfb_sleep.c \
        drvlfb_malloc.c \
        hankaku_font.c

OBJS	= $(SRCS:.c=.o)
PROG	= ../../../libblike.a
DEFS	=
LIBS	= -lpthread \
	-lm

INCS	= -I $(INCLUDE_BASE_PATH) \
	-I $(INCLUDE_BASE_PATH)/common

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

