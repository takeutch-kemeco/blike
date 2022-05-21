/* c_blike_01f_linux -
 * Copyright (C) 2011, 2012, 2013 Kemeco Takeutch <takeutchkemeco@gmail.com>
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

#include <blike0.h>
#include <stdarg.h>
#include "setjmp.h"

int blMain();

int bl_main();
void bl_putchar8b(int x, int y, int n, int c, int b);
void bl_initFont();
void bl_putKeyB(int n, int *p);
void bl_readyWin();
extern unsigned char hankaku[4096];

void bld_openWin(int sx, int sy);
void bld_flshWin(int sx, int sy, int x0, int y0);
void bld_waitNF();
void bld_flshSys();
int bld_getSeed();
void *bld_malloc(unsigned int bytes);
void bld_free(void *p, unsigned int bytes);
int bld_vsnprintf(char *b, int n, const char *f, va_list ap);
void bld_initFont();
int bld_maxfonts();
void bld_lock();
void bld_unlock();

struct BL_WIN {
        int xsiz, ysiz, *buf;
};

struct BL_WORK {
        struct BL_WIN win[19];
        jmp_buf jb;
        int csiz_x, csiz_y, cx, cy, col0, col1, tabsiz, slctwin;
        int tmcount, tmcount0, mod, rand_seed;
        int *cbuf;
        unsigned char *ftyp;
        unsigned char **fptn;
        int *ccol, *cbak;
        int *kbuf, kbuf_rp, kbuf_wp, kbuf_c;
};

extern struct BL_WORK bl_work;

#if (!defined(NULL))
        #define NULL ((void *) 0)
#endif

#define BL_SIZ_KBUF 8192

#define BL_READYFONTS    0x40000000
#define BL_READYRANDSEED 0x20000000

#define BL_DBGWIN  16
#define BL_SLCTWIN 17
#define BL_SYSWIN  18

#define BL_READY_WINDOW if (bl_work.win[BL_SLCTWIN].xsiz == 0) bl_readyWin(bl_work.slctwin);
#define BL_READY_WINDOW0 if (bl_work.win[0].xsiz == 0) bl_readyWin(0);
#define BL_READY_FONTS if ((bl_work.mod & BL_READYFONTS) == 0) bld_initFont();
#define BL_EXIT longjmp(bl_work.jb, 1);
