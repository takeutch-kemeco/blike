CFLAGS = -I/usr/include/blike
LIBS = `pkg-config blike --libs`

all:
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test000a.hs hs_bl_wrapper.c $(LIBS) -o hs_test000a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test001a.hs hs_bl_wrapper.c $(LIBS) -o hs_test001a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test002a.hs hs_bl_wrapper.c $(LIBS) -o hs_test002a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test003a.hs hs_bl_wrapper.c $(LIBS) -o hs_test003a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test004a.hs hs_bl_wrapper.c $(LIBS) -o hs_test004a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test005a.hs hs_bl_wrapper.c $(LIBS) -o hs_test005a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test006a.hs hs_bl_wrapper.c $(LIBS) -o hs_test006a

	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test008a.hs hs_bl_wrapper.c $(LIBS) -o hs_test008a
 
clean:
	rm -f hs_test{000,001,002,003,004,005,006,008}a{,.o,.hi,_stub.h} blike{.o,.hi} hs_bl_wrapper.o

