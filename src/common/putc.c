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

void bl_scroll16()
{
        return;
}

void bl_putc(int c)
{
        BL_READY_WINDOW
        BL_READY_FONTS
        if (w.cx + 1 >= w.csiz_x) {
                w.cx = 0;
                w.cy += 2;
        }
        if (w.cy + 2 >= w.csiz_y)
                bl_scroll16();
        if (c == '\r') {
                w.cx = 0;
        } else if (c == '\n') {
                w.cx = 0;
                w.cy += 2;
        } else if (c == '\t') {
                do {
                        bl_putchar8b(w.cx, w.cy + 0, ' ', w.col0, w.col1);
                        bl_putchar8b(w.cx, w.cy + 1, ' ' + 256, w.col0, w.col1);
                        w.cx++;
                } while (w.cx % w.tabsiz != 0);
        } else {
                bl_putchar8b(w.cx, w.cy + 0, c, w.col0, w.col1);
                bl_putchar8b(w.cx, w.cy + 1, c + 256, w.col0, w.col1);
                w.cx++;
        }
        return;
}
