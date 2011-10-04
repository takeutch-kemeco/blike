CC = gcc
AR = ar

INCLUDE_BASE_PATH = ../../../include

SRCS =	bl_complex.c \
	bl_dft.c

OBJS	= $(SRCS:.c=.o)
PROG	= ../../../libblike.a
DEFS	=
LIBS	=	
INCS	= -I $(INCLUDE_BASE_PATH)/lib/bl_math
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

