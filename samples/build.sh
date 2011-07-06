#!/bin/bash

#cd ../lib_src/common
#make clean
#make

cd ../lib_src/gtk
make clean
make

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

A=test000a;	make SRCS="$A".c PROG=$A
A=test001a;	make SRCS="$A".c PROG=$A
A=test002a;	make SRCS="$A".c PROG=$A
A=test003a;	make SRCS="$A".c PROG=$A
A=test004a;	make SRCS="$A".c PROG=$A
A=test005a;	make SRCS="$A".c PROG=$A
A=test006a;	make SRCS="$A".c PROG=$A
A=test007a;	make SRCS="$A".c PROG=$A
A=test008a;	make SRCS="$A".c PROG=$A
A=test009a;	make SRCS="$A".c PROG=$A
A=test010a;	make SRCS="$A".c PROG=$A
A=test011a;	make SRCS="$A".c PROG=$A
A=test012a;	make SRCS="$A".c PROG=$A
A=test013a;	make SRCS="$A".c PROG=$A
A=test014a;	make SRCS="$A".c PROG=$A
A=test015a;	make SRCS="$A".c PROG=$A
A=test016a;     make SRCS="$A".c PROG=$A
A=test017a;     make SRCS="$A".c PROG=$A
A=test018a;     make SRCS="$A".c PROG=$A
A=test019a;     make SRCS="$A".c PROG=$A
A=test020a;     make SRCS="$A".c PROG=$A
A=test021a;     make SRCS="$A".c PROG=$A

A=bball00a;	make SRCS="$A".c PROG=$A
A=boxes00a;	make SRCS="$A".c PROG=$A
A=kcube00a;	make SRCS="$A".c PROG=$A
A=msgk00a;	make SRCS="$A".c PROG=$A
A=msgk01a;	make SRCS="$A".c PROG=$A
A=mt_xor0a;	make SRCS="$A".c PROG=$A

A=test_keybord_state;	make SRCS="$A".c PROG=$A
A=test_bl3d_triangle;	make SRCS="$A".c PROG=$A
A=bl3d_cube;		make SRCS="$A".c PROG=$A

