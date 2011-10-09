CC = gcc
AR = ar

BASE_PATH = ../../..
INCLUDE_BASE_PATH = $(BASE_PATH)/include

SRCS	= bl_main_wrapper.c
OBJS	= $(SRCS:.c=.o)
PROG	= bl_main_wrapper.a
DEFS	=
LIBS	=	
INCS	=
OPTS	= -O2

all: $(PROG)

$(PROG): $(OBJS)
	$(AR) -rv $@ $(OBJS)

.c.o:
	$(CC) $(OPTS) $(DEFS) $(INCS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

