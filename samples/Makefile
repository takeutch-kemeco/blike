SRCS =
OBJS = $(SRCS:.c=.o)
PROG =

CFLAGS = `pkg-config blike --cflags`
LIBS = `pkg-config blike --libs`

all:
	gcc -DHAVE_CONFIG_H $(CFLAGS) -o $(PROG) $(SRCS) $(LIBS)

clean:
	rm -f $(OBJS)
	rm -f $(PROG)

