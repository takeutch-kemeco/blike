CC = gcc
AR = ar

INCLUDE_BASE_PATH = ../../../include

SRCS = bl3d_system.c \
	bl3d_matrix.c \
	bl3d_math.c \
	bl3d_dobj.c \
	bl3d_packet_pool.c \
	bl3d_frame_buffer.c \
	bl3d_triangle.c

OBJS	= $(SRCS:.c=.o)
PROG	= ../../../libblike.a
DEFS	=
LIBS	= `pkg-config glib-2.0 --libs` \
	-lm
	
INCS	= -I $(INCLUDE_BASE_PATH) \
	-I $(INCLUDE_BASE_PATH)/lib/bl3d \
	`pkg-config glib-2.0 --cflags`

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

