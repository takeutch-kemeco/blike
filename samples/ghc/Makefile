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
 
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test010a.hs hs_bl_wrapper.c $(LIBS) -o hs_test010a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test011a.hs hs_bl_wrapper.c $(LIBS) -o hs_test011a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test012a.hs hs_bl_wrapper.c $(LIBS) -o hs_test012a

	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test015a.hs hs_bl_wrapper.c $(LIBS) -o hs_test015a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test015b.hs hs_bl_wrapper.c $(LIBS) -o hs_test015b
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test016a.hs hs_bl_wrapper.c $(LIBS) -o hs_test016a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test017a.hs hs_bl_wrapper.c $(LIBS) -o hs_test017a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test018a.hs hs_bl_wrapper.c $(LIBS) -o hs_test018a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test020a.hs hs_bl_wrapper.c $(LIBS) -o hs_test020a
	ghc --make -no-hs-main $(CFLAGS) blike.hs hs_test021a.hs hs_bl_wrapper.c $(LIBS) -o hs_test021a

clean:
	rm -f hs_test{000,001,002,003,004,005,006,008,010,011,012,015,016,017,018,020,021}?{,.o,.hi,_stub.h} blike{.o,.hi} hs_bl_wrapper.o

