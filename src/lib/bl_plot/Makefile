CC = gcc

BASE_PATH = ../../..
INCLUDE_BASE_PATH = $(BASE_PATH)/include

SRCS =	bl_plot.c
OBJS	= $(SRCS:.c=.o)
SONAME	= libbl_plot.so
PROG	= $(SONAME)
DEFS	=
LIBS	=	
INCS	= -I $(INCLUDE_BASE_PATH) \
	-I $(INCLUDE_BASE_PATH)/lib/bl_plot

OPTS	= -O2

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) -shared -Wl,-soname,$(SONAME) -o $@ $(OBJS) $(LIBS)

.c.o:
	$(CC) -fPIC $(OPTS) $(DEFS) $(INCS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

