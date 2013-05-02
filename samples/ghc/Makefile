CFLAGS = -I/usr/include/blike
LIBS = `pkg-config blike --libs`

all:
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test000a.hs hs_bl_wrapper.c $(LIBS) -o hs_test000a
 
clean:
	rm -f hs_test000a{,.o,.hi,_stub.h} blike{.o,.hi} hs_bl_wrapper.o

