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

#include <blikedrv.h>

#define w bl_work

void bl_copyRct0(int sx, int sy, int n0, int x0, int y0, int n1, int x1, int y1)
{
        int x, y, sx0, sx1, *b0, *b1;
        if (w.win[n0].xsiz == 0) bl_readyWin(n0);
        if (w.win[n1].xsiz == 0) bl_readyWin(n1);
        if (x0 < 0) {
                sx += x0;
                x1 -= x0;
                x0 = 0;
        }
        if (y0 < 0) {
                sy += y0;
                y1 -= y0;
                y0 = 0;
        }
        if (sx > w.win[n0].xsiz - x0)
                sx = w.win[n0].xsiz - x0;
        if (sy > w.win[n0].ysiz - y0)
                sy = w.win[n0].ysiz - y0;
        if (x1 < 0) {
                sx += x1;
                x0 -= x1;
                x1 = 0;
        }
        if (y1 < 0) {
                sy += y1;
                y0 -= y1;
                y1 = 0;
        }
        if (sx > w.win[n1].xsiz - x1)
                sx = w.win[n1].xsiz - x1;
        if (sy > w.win[n1].ysiz - y1)
                sy = w.win[n1].ysiz - y1;
        b0 = w.win[n0].buf; sx0 = w.win[n0].xsiz;
        b1 = w.win[n1].buf; sx1 = w.win[n1].xsiz;
        for (y = 0; y < sy; y++) {
                for (x = 0; x < sx; x++)
                        b1[(x1 + x) + (y1 + y) * sx1] = b0[(x0 + x) + (y0 + y) * sx0];
        }
        return;
}
