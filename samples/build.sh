#!/bin/bash

cd ../lib_src/gtk
make clean
make -j 2

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

rm -f test_spline

rm -f test_complex
rm -f test_dft

rm -f test_lua

A=test000a;	make SRCS="$A".c PROG=$A -j 2
A=test001a;	make SRCS="$A".c PROG=$A -j 2
A=test002a;	make SRCS="$A".c PROG=$A -j 2
A=test003a;	make SRCS="$A".c PROG=$A -j 2
A=test004a;	make SRCS="$A".c PROG=$A -j 2
A=test005a;	make SRCS="$A".c PROG=$A -j 2
A=test006a;	make SRCS="$A".c PROG=$A -j 2
A=test007a;	make SRCS="$A".c PROG=$A -j 2
A=test008a;	make SRCS="$A".c PROG=$A -j 2
A=test009a;	make SRCS="$A".c PROG=$A -j 2
A=test010a;	make SRCS="$A".c PROG=$A -j 2
A=test011a;	make SRCS="$A".c PROG=$A -j 2
A=test012a;	make SRCS="$A".c PROG=$A -j 2
A=test013a;	make SRCS="$A".c PROG=$A -j 2
A=test014a;	make SRCS="$A".c PROG=$A -j 2
A=test015a;	make SRCS="$A".c PROG=$A -j 2
A=test016a;     make SRCS="$A".c PROG=$A -j 2
A=test017a;     make SRCS="$A".c PROG=$A -j 2
A=test018a;     make SRCS="$A".c PROG=$A -j 2
A=test019a;     make SRCS="$A".c PROG=$A -j 2
A=test020a;     make SRCS="$A".c PROG=$A -j 2
A=test021a;     make SRCS="$A".c PROG=$A -j 2

A=bball00a;	make SRCS="$A".c PROG=$A -j 2
A=boxes00a;	make SRCS="$A".c PROG=$A -j 2
A=kcube00a;	make SRCS="$A".c PROG=$A -j 2
A=msgk00a;	make SRCS="$A".c PROG=$A -j 2
A=msgk01a;	make SRCS="$A".c PROG=$A -j 2
A=mt_xor0a;	make SRCS="$A".c PROG=$A -j 2

A=test_keybord_state;	make SRCS="$A".c PROG=$A -j 2
A=test_bl3d_triangle;	make SRCS="$A".c PROG=$A -j 2
A=bl3d_cube;		make SRCS="$A".c PROG=$A -j 2
A=bl3d_cube2;		make SRCS="$A".c PROG=$A -j 2

A=test_bl_plot;		make SRCS="$A".c PROG=$A -j 2

A=test_spline;		make SRCS="$A".c PROG=$A -j 2

A=test_complex;		make SRCS="$A".c PROG=$A -j 2
A=test_dft;		make SRCS="$A".c PROG=$A -j 2

A=test_lua;		make SRCS="$A".c PROG=$A -j 2

