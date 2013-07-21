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

void bl_fillOval(int sx, int sy, int x0, int y0)
{
        int x, y;
        double cx = sx * 0.5 + 0.5, cy = sy * 0.5 + 0.5, dx2, dy2, r2 = cx * cy;
        r2 *= r2;
        for (y = 0; y < sy; y++) {
                dy2 = (y - cy + 1) * cx;
                dy2 *= dy2;
                for (x = 0; x < sx; x++) {
                        dx2 = (x - cx + 1) * cy;
                        dx2 *= dx2;
                        if (dx2 + dy2 <= r2)
                                bl_setPix(x0 + x, y0 + y, w.col0);
                }
        }
        return;
}
