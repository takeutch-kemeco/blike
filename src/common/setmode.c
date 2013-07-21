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

void bl_setMode(int mod)
{
        int i;
        if ((mod & (BL_PSET & BL_PAND & BL_POR & BL_PXOR)) != 0) {
                w.mod &= ~(BL_PSET | BL_PAND | BL_POR | BL_PXOR);
                w.mod |= mod & (BL_PSET | BL_PAND | BL_POR | BL_PXOR);
        }
        if ((mod & (BL_FULLHEIGHT & BL_HALFHEIGHT)) != 0) {
                w.mod &= ~(BL_FULLHEIGHT | BL_HALFHEIGHT);
                w.mod |= mod & (BL_FULLHEIGHT | BL_HALFHEIGHT);
        }
        if ((mod & (BL_DBGFLSH & BL_RLSFLSH)) != 0) {
                w.mod &= ~(BL_DBGFLSH | BL_RLSFLSH);
                w.mod |= mod & (BL_DBGFLSH | BL_RLSFLSH);
                i = mod & (BL_DBGFLSH | BL_RLSFLSH);
                if (i == BL_DBGFLSH)
                        w.win[BL_SYSWIN] = w.win[BL_DBGWIN];
                if (i == BL_RLSFLSH)
                        w.win[BL_SYSWIN] = w.win[0];
        }
        return;
}
