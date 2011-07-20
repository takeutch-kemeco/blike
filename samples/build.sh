#!/bin/bash

cd ../lib_src/gtk
make clean
make -j 2

cd ../../samples
rm *.o
rm test000a
rm test001a
rm test002a
rm test003a
rm test004a
rm test005a
rm test006a
rm test008a
rm test010a
rm test011a
rm test012a
rm test013a
rm test014a
rm test015a
rm test016a
rm test017a
rm test018a
rm test019a
rm test020a
rm test021a

rm bball00a
rm boxes00a
rm kcube00a
rm mt_xor0a

rm msgk00a
rm msgk01a

rm test_keybord_state
rm test_bl3d_triangle 

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
