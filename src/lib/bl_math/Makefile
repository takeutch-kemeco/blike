CC = gcc

BASE_PATH = ../../..
INCLUDE_BASE_PATH = $(BASE_PATH)/include

SRCS =	bl_complex.c \
	bl_dft.c

OBJS	= $(SRCS:.c=.o)
SONAME	= libbl_math.so
PROG	= $(SONAME)
DEFS	=
LIBS	= -lm
INCS	= -I $(INCLUDE_BASE_PATH)/lib/bl_math
OPTS	= -O2

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) -shared -Wl,-soname,$(SONAME) -o $@ $(OBJS) $(LIBS)

.c.o:
	$(CC) -fPIC $(OPTS) $(DEFS) $(INCS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

