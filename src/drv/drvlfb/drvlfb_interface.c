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

#include <string.h>
#include <stdint.h>
#include <time.h>

#include "drvlfb_system.h"
#include "drvlfb_sleep.h"
#include "drvlfb_malloc.h"
#include <blikedrv.h>
#include <blike0.h>

extern struct BL_WORK __attribute__((aligned(16))) bl_work;

void bld_openWin(int x, int y)
{
        drvlfb_system->window_width  = x;
        drvlfb_system->window_height = y;
}

static inline uint32_t* fb_seek_pix_adrs(const int x, const int y)
{
        return drvlfb_system->smem +
               (y * drvlfb_system->bytePerLine) + (x * 4);
}

static void fb_set_pixel(const int x, const int y, const uint32_t col)
{
        if((x >= 0 && x < drvlfb_system->screen_width) &&
           (y >= 0 && y < drvlfb_system->screen_height)) {
                uint32_t* d = fb_seek_pix_adrs(x, y);
                *d = col;
        }
}

static void fb_draw_raw_image(unsigned char* img,
                              const int offx, const int offy)
{
        uint32_t *p = (uint32_t*)img;

        int j = 0;
        for (j = 0; j < drvlfb_system->window_height; j++) {
                int i = 0;
                for (i = 0; i < drvlfb_system->window_width; i++) {
                        fb_set_pixel(offx + i, offy + j, *p);
                        p++;
                }
        }
}

void bld_flshWin(int sx, int sy, int x0, int y0)
{
        const int offset_x =
                (drvlfb_system->screen_width - drvlfb_system->window_width) / 2;

        const int offset_y =
                (drvlfb_system->screen_height - drvlfb_system->window_height) / 2;

        unsigned char* p = (unsigned char*)(bl_work.win[0].buf);

        fb_draw_raw_image(p, offset_x, offset_y);
}

void bld_flshSys()
{
        fflush(stdout);
}

void bld_waitNF()
{
        drvlfb_msleep(DRVLFB_SYGNAL_CHECK_INTERVAL);
        bl_work.tmcount += DRVLFB_SYGNAL_CHECK_INTERVAL;
}

void bld_exit()
{
        drvlfb_close_system(drvlfb_system);
}

int bld_getSeed()
{
        return (int)time(NULL);
}

void* bld_malloc(unsigned int bytes)
{
        return drvlfb_malloc_aligned16((size_t)bytes);
}

void* __bld_mallocRWE(unsigned int bytes)
{
        return drvlfb_malloc_rwe((size_t)bytes);
}

void bld_free(void* p, unsigned int bytes)
{
        drvlfb_free_aligned16((void*)p);
}

extern unsigned char hankaku[4096];
void bld_initFont()
{
        bl_initFont();
        bl_work.mod |= BL_READYFONTS;
        return;
}

int bld_maxfonts()
{
        return 65536 * 4;
}

int bld_vsnprintf(char *b, int n, const char *f, va_list ap)
{
        vsnprintf(b, n, f, ap);
}

void bld_lock()
{
}

void bld_unlock()
{
}
