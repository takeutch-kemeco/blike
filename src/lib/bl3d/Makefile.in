CC = gcc

BASE_PATH = ../../..
INCLUDE_BASE_PATH = $(BASE_PATH)/include

SRCS = bl3d_system.c \
	bl3d_matrix.c \
	bl3d_math.c \
	bl3d_dobj.c \
	bl3d_packet_pool.c \
	bl3d_frame_buffer.c \
	bl3d_triangle.c

OBJS	= $(SRCS:.c=.o)
SONAME	= libbl3d.so
PROG	= $(SONAME)
DEFS	=
LIBS	= `pkg-config glib-2.0 --libs` \
	-lm
	
INCS	= -I $(INCLUDE_BASE_PATH) \
	-I $(INCLUDE_BASE_PATH)/lib/bl3d \
	`pkg-config glib-2.0 --cflags`

OPTS	= -O2

all: $(PROG)

$(PROG): $(OBJS)
	$(CC) -shared -Wl,-soname,$(SONAME) -o $@ $(OBJS) $(LIBS)

.c.o:
	$(CC) -fPIC $(OPTS) $(DEFS) $(INCS) -c -o $@ $<

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

