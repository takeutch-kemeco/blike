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

void bl_openVWin(int n, int sx, int sy)
{
        int i, x, y;
        if (n < 0 || n > BL_DBGWIN) return;
        if (w.win[n].xsiz != 0) return;
        if (sx <= 0 || sy <= 0) return;
        if (n == BL_DBGWIN) {
                if (w.win[0].xsiz == 0) return; /* 先に作ってはいけない */
                if (sx != w.win[0].xsiz || sy != w.win[0].ysiz) return;
        }
        if (n == 0) {
                if (sx < 160) return;
                w.cbuf = NULL;
                w.ccol = NULL;
                w.cbak = NULL;
        }
        w.win[n].buf = bld_malloc(sx * sy * sizeof (int));
        if (w.win[n].buf == NULL) {
err:
                BL_EXIT
        }
        w.win[n].xsiz = sx;
        w.win[n].ysiz = sy;
        if (n == 0) {
                w.csiz_x = (sx + 7) / 8;
                w.csiz_y = (sy + 7) / 8;
                w.cbuf = bld_malloc(w.csiz_x * w.csiz_y * sizeof (int));
                w.ccol = bld_malloc(w.csiz_x * w.csiz_y * sizeof (int));
                w.cbak = bld_malloc(w.csiz_x * w.csiz_y * sizeof (int));
                if (w.cbuf == NULL || w.ccol == NULL || w.cbak == NULL)
                        goto err;
        }
        if (n != BL_DBGWIN) {
                i = w.slctwin;
                w.slctwin = n;
                w.win[BL_SLCTWIN] = w.win[n];
                bl_cls();
                w.slctwin = i;
                w.win[BL_SLCTWIN] = w.win[i];
        } else {
                /* わざとゴミで埋める */
                i = w.slctwin;
                w.slctwin = n;
                w.win[BL_SLCTWIN] = w.win[n];
                for (y = 0; y < sy; y++) {
                        for (x = 0; x < sx; x++)
                                w.win[BL_SLCTWIN].buf[x + y * sx] = bl_iCol((x + y) & 7);
                }
                w.slctwin = i;
                w.win[BL_SLCTWIN] = w.win[i];
        }
        i = w.mod & (BL_DBGFLSH | BL_RLSFLSH);
        if (i == BL_DBGFLSH)
                w.win[BL_SYSWIN] = w.win[BL_DBGWIN];
        if (i == BL_RLSFLSH)
                w.win[BL_SYSWIN] = w.win[0];
        if (n == 0)
                bld_openWin(sx, sy);
        if (n == BL_DBGWIN) {
                i = w.mod & (BL_DBGFLSH | BL_RLSFLSH);
                if (i == BL_DBGFLSH)
                        bld_flshWin(sx, sy, 0, 0);
        }
        return;
}
