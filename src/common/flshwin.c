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

void bl_flshWin(int sx, int sy, int x0, int y0)
{
        int x, y;
        BL_READY_WINDOW0
        if (x0 < 0) {
                sx += x0;
                x0 = 0;
        }
        if (y0 < 0) {
                sy += y0;
                y0 = 0;
        }
        if (sx > w.win[0].xsiz - x0)
                sx = w.win[0].xsiz - x0;
        if (sy > w.win[0].ysiz - y0)
                sy = w.win[0].ysiz - y0;
        if (sx > 0 && sy > 0) {
                if ((w.mod & (BL_DBGFLSH | BL_RLSFLSH)) == BL_DBGFLSH) {
                        if (w.win[BL_DBGWIN].xsiz == 0)
                                bl_openVWin(BL_DBGWIN, w.win[0].xsiz, w.win[0].ysiz);
                        bl_copyRct0(sx, sy, 0, x0, y0, BL_DBGWIN, x0, y0);
                        for (y = 0; y < sy; y++) {
                                for (x = 0; x < sx; x++) {
                                        if (((unsigned int) w.win[0].buf[(x0 + x) + (y0 + y) * w.win[0].xsiz]) > 0xffffff)
                                                goto err;
                                }
                        }
                        /* 不正な色をwin[0]で使用してはいけない */
                        /* 本来は描画段階ではねるべきだが(そのほうが見つけやすい)、それだとチェックが重くなるので、BL_DBGWINへの反映を阻止するという方法にした */
                        /* チェックが重くなるというのは、チェック処理が遅いという意味ではなくて、チェックのための記述が多くなるという意味 */
                        /* デバッグモードなので、処理が遅いのはたいした問題ではない */
                }
                bld_flshWin(sx, sy, x0, y0);
        }
err:
        return;
}
