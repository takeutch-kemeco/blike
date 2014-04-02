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

#include "blikedrv.h"

#define w bl_work

void setPix0(int x, int y, int c)
{
        if (0 <= x && x < w.win[BL_SLCTWIN].xsiz && 0 <= y && y < w.win[BL_SLCTWIN].ysiz)
                w.win[BL_SLCTWIN].buf[x + y * w.win[BL_SLCTWIN].xsiz] = c;
        return;
}

void bl_putchar8b(int x, int y, int n, int c, int b)
{
        int i;
        unsigned char *p = w.fptn[n];
        if (0 <= x && x <= w.csiz_x && 0 <= y && y <= w.csiz_y) {
                w.cbuf[x + y * w.csiz_x] = n;
                w.ccol[x + y * w.csiz_x] = c;
                w.cbak[x + y * w.csiz_x] = b;
                for (i = 0; i < 8; i++) {
                        if ((p[i] & 0x80) != 0) setPix0(x * 8 + 0, y * 8 + i, c); else setPix0(x * 8 + 0, y * 8 + i, b);
                        if ((p[i] & 0x40) != 0) setPix0(x * 8 + 1, y * 8 + i, c); else setPix0(x * 8 + 1, y * 8 + i, b);
                        if ((p[i] & 0x20) != 0) setPix0(x * 8 + 2, y * 8 + i, c); else setPix0(x * 8 + 2, y * 8 + i, b);
                        if ((p[i] & 0x10) != 0) setPix0(x * 8 + 3, y * 8 + i, c); else setPix0(x * 8 + 3, y * 8 + i, b);
                        if ((p[i] & 0x08) != 0) setPix0(x * 8 + 4, y * 8 + i, c); else setPix0(x * 8 + 4, y * 8 + i, b);
                        if ((p[i] & 0x04) != 0) setPix0(x * 8 + 5, y * 8 + i, c); else setPix0(x * 8 + 5, y * 8 + i, b);
                        if ((p[i] & 0x02) != 0) setPix0(x * 8 + 6, y * 8 + i, c); else setPix0(x * 8 + 6, y * 8 + i, b);
                        if ((p[i] & 0x01) != 0) setPix0(x * 8 + 7, y * 8 + i, c); else setPix0(x * 8 + 7, y * 8 + i, b);
                }
        }
}
