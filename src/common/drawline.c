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

void bl_drawLine(int x0, int y0, int x1, int y1)
{
        int i, x, y, len, dx, dy;

        dx = x1 - x0;
        dy = y1 - y0;
        x = x0 << 10;
        y = y0 << 10;
        if (dx < 0) {
                dx = - dx;
        }
        if (dy < 0) {
                dy = - dy;
        }
        if (dx >= dy) {
                len = dx + 1;
                if (x0 > x1) {
                        dx = -1024;
                } else {
                        dx =  1024;
                }
                if (y0 <= y1) {
                        dy = ((y1 - y0 + 1) << 10) / len;
                } else {
                        dy = ((y1 - y0 - 1) << 10) / len;
                }
        } else {
                len = dy + 1;
                if (y0 > y1) {
                        dy = -1024;
                } else {
                        dy =  1024;
                }
                if (x0 <= x1) {
                        dx = ((x1 - x0 + 1) << 10) / len;
                } else {
                        dx = ((x1 - x0 - 1) << 10) / len;
                }
        }

        for (i = 0; i < len; i++) {
                bl_setPix(x >> 10, y >> 10, w.col0);
                x += dx;
                y += dy;
        }

        return;
}
