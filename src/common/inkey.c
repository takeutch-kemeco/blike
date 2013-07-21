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

int bl_inkey1()
{
        return bl_inkey(BL_GETKEY | BL_CLEARREP | BL_DELFFF);
}

int bl_inkey(int flags)
{
        int i, j;
        if ((flags & BL_WAITKEYF) != 0) {
                BL_READY_WINDOW0
                bl_flshWin(w.win[0].xsiz, w.win[0].ysiz, 0, 0);
        }
        do {
                if ((flags & BL_WAITKEYNF) != 0) {
                        while (w.kbuf_c <= 0)
                                bl_waitNF(10);
                }
                if ((flags & BL_CLEARREP) != 0 && w.kbuf_c >= 2) {
                        i = w.kbuf[w.kbuf_rp];
                        if (0 < i && i < 0x0fff) {
                                do {
                                        j = (w.kbuf_rp + 1) & ~(BL_SIZ_KBUF - 1);
                                        if (i != w.kbuf[j])
                                                break;
                                        bld_lock();
                                        w.kbuf_c--;
                                        w.kbuf_rp = (w.kbuf_rp + 1) & ~(BL_SIZ_KBUF - 1);
                                        bld_unlock();
                                } while (w.kbuf_c >= 2);
                        }
                }
                i = 0;
                if (w.kbuf_c > 0) {
                        i = w.kbuf[w.kbuf_rp];
                        if ((flags & BL_GETKEY) != 0) {
                                bld_lock();
                                w.kbuf_c--;
                                w.kbuf_rp = (w.kbuf_rp + 1) & ~(BL_SIZ_KBUF - 1);
                                bld_unlock();
                        }
                }
        } while ((flags & BL_DELFFF) != 0 && i == 0x0fff);
        return i;
}

