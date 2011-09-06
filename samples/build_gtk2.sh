#!/bin/bash

cd ../lib_src/gtk
make -f Makefile.gtk2 clean
make -f Makefile.gtk2 -j 2

cd ../../samples
rm -f *.o
rm -f test000a
rm -f test001a
rm -f test002a
rm -f test003a
rm -f test004a
rm -f test005a
rm -f test006a
rm -f test008a
rm -f test010a
rm -f test011a
rm -f test012a
rm -f test013a
rm -f test014a
rm -f test015a
rm -f test016a
rm -f test017a
rm -f test018a
rm -f test019a
rm -f test020a
rm -f test021a

rm -f bball00a
rm -f boxes00a
rm -f kcube00a
rm -f mt_xor0a

rm -f msgk00a
rm -f msgk01a

rm -f test_keybord_state
rm -f test_bl3d_triangle 
rm -f bl3d_cube
rm -f bl3d_cube2

rm -f test_bl_plot

A=test000a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test001a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test002a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test003a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test004a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test005a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test006a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test007a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test008a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test009a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test010a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test011a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test012a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test013a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test014a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test015a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test016a;     make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test017a;     make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test018a;     make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test019a;     make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test020a;     make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test021a;     make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2

A=bball00a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=boxes00a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=kcube00a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=msgk00a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=msgk01a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=mt_xor0a;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2

A=test_keybord_state;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=test_bl3d_triangle;	make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=bl3d_cube;		make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2
A=bl3d_cube2;		make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2

A=test_bl_plot;		make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2

A=test_spline;		make -f Makefile.gtk2 SRCS="$A".c PROG=$A -j 2

