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

#define w       bl_work

void bl_gets(char *s)
/* せいぜい1024バイトしか入力できない, 改行は残る */
{
        int mod0 = w.mod, x0 = w.cx, y0 = w.cy, c, l = 0, i;
        bl_setMode(BL_PXOR);
        s[0] = '\0';
        for (;;) {
                bl_fillRect(2, 16, w.cx * 8, (y0 / 2) * 16);
                c = bl_inkey(BL_WAITKEY | BL_GETKEY | BL_DELFFF);
                bl_fillRect(2, 16, w.cx * 8, (y0 / 2) * 16);

                if (' ' <= c && c <= 0x7e && w.cx < w.csiz_x - 1) {
                        l++;
                        for (i = l; i > w.cx - x0; i--)
                                s[i] = s[i - 1];
                        s[w.cx - x0] = c;

                        w.cx++;
                }

                if (c == KEY_LEFT && w.cx > x0)
                        w.cx--;

                if (c == KEY_RIGHT && w.cx - x0 < l)
                        w.cx++;

                if (c == KEY_BACKSPACE && w.cx - x0 > 0) {
                        l--;
                        i = w.cx;
                        bl_locate(x0 + l, y0 / 2);
                        bl_putc(' ');
                        w.cx = i - 1;
                        for (i = w.cx - x0; i <= l; i++)
                                s[i] = s[i + 1];
                }

                if (c == KEY_DEL && w.cx - x0 < l && l > 0) {
                        l--;
                        i = w.cx;
                        bl_locate(x0 + l, y0 / 2);
                        bl_putc(' ');
                        w.cx = i;
                        for (i = w.cx - x0; i <= l; i++)
                                s[i] = s[i + 1];
                }

                if (c == KEY_ENTER)
                        break;

                i = w.cx;
                bl_locate(x0, y0 / 2);
                bl_puts(s);
                w.cx = i;
        }
        w.mod = mod0;
        bl_putc('\n');
        return;
}
