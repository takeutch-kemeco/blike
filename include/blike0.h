/* c_blike_01f_linux -
 * Copyright (C) 2011, 2012, 2013, 2022 Kemeco Takeutch <takeutchkemeco@gmail.com>
 * All rights reserved.
 *
 * c_blike_01f -
 * Copyright (C) 2011 H.Kawai (under KL-01)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name of the Kemeco Takeutch nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#pragma once

#include <stdarg.h>

#define BLIKE0_H 1

#if (defined(__cplusplus))
extern "C" {
#endif

void bl_openWin(int x, int y);
void bl_putc(int c);
void bl_puts(const char *s);
void bl_puts1(const char *s);
void bl_setCol(int c);
void bl_setBCol(int c);
int bl_rbg(int r, int g, int b);
int bl_iCol(int i);
void bl_flshWin(int sx, int sy, int x0, int y0);
int *bl_getGrpB();
void bl_setPix(int x, int y, int c);
void bl_fillRect(int sx, int sy, int x0, int y0);
void bl_drawRect(int sx, int sy, int x0, int y0);
void bl_drawLine(int x0, int y0, int x1, int y1);
int bl_rnd(int max_puls_1);
void bl_wait(int msec);
void bl_color(int c, int b);
void bl_locate(int x, int y);
void bl_printf(const char *s, ...);
int bl_getPix(int x, int y);
void bl_waitNF(int msec);
int bl_inkey1();
void bl_cls();
unsigned int bl_clock();
int bl_inptInt(const char *s, ...);
double bl_inptFlot(const char *s, ...);
int bl_scanf(const char *s, ...);
int bl_vscanf(const char *f, va_list ap);
void bl_setMode(int mod);
int bl_rand();
void bl_srand(int seed);
int bl_inkey(int flags);
void bl_gets(char *s);
void bl_fillOval(int sx, int sy, int x0, int y0);
void bl_drawStr(int x0, int y0, int rx, int ry, const char *s, ...);
void bl_openVWin(int n, int x, int y);
void bl_slctWin(int n);
void bl_copyRct0(int sx, int sy, int n0, int x0, int y0, int n1, int x1, int y1);
void bl_copyRct1(int sx, int sy, int n0, int x0, int y0, int n1, int x1, int y1, int ic);
void bl_drawPtrn_r(int sx, int sy, int x0, int y0, const char *c, const char *p);
void bl_drawPtrn_d(int sx, int sy, int x0, int y0, const char *c, const char *p);

void bl_readyWin(int n);
void bl_setPtrn0(int sx, int sy, int sl, int ic, int *b, const unsigned char *c, const unsigned char *p, void (*errfnc)(const char *msg, unsigned char *nam));
void bl_drawPtrn_err_r(const char *msg, unsigned char *nam);
void bl_drawPtrn_err_d(const char *msg, unsigned char *nam);
void bl_leapFlush(int msec);

/* 起動時のコマンドライン引数 main(bl_argc, bl_argv) */
extern int bl_argc;
extern char** bl_argv;

/* void bl_setFlsW(int w0, int ppw); */

/* intは32bit, shortが16bit, charが8bit */

#define KEY_ENTER       '\n'
#define KEY_ESC         27
#define KEY_BACKSPACE   8
#define KEY_TAB         9
#define KEY_PAGEUP      0x1020
#define KEY_PAGEDWN     0x1021
#define KEY_END         0x1022
#define KEY_HOME        0x1023
#define KEY_LEFT        0x1024
#define KEY_UP          0x1025
#define KEY_RIGHT       0x1026
#define KEY_DOWN        0x1027
#define KEY_INS         0x1028
#define KEY_DEL         0x1029

#define BL_PSET         0x00000004
#define BL_PAND         0x00000005
#define BL_POR          0x00000006
#define BL_PXOR         0x00000007
#define BL_FULLHEIGHT   0x00000010
#define BL_HALFHEIGHT   0x00000018
#define BL_DBGFLSH      0x00000040
#define BL_RLSFLSH      0x00000060
#define BL_DEBUG        BL_DBGFLSH
#define BL_RELEASE      BL_RLSFLSH

#define BL_WAITKEYF     0x00000001
#define BL_WAITKEYNF    0x00000002
#define BL_WAITKEY      0x00000003
#define BL_GETKEY       0x00000004
#define BL_CLEARREP     0x00000008
#define BL_DELFFF       0x00000010

#define BL_KEYMODE      0x00000000      /* 作りかけ, make/remake/breakが見えるかどうか */

#if (defined(__cplusplus))
}
#endif
