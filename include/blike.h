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

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <blike0.h>

#ifndef __BLIKE_H__
#define __BLIKE_H__

#define BLIKE_H 1

#if (defined(__cplusplus))
extern "C" {
#endif

#define putc            bl_putc
#define puts            bl_puts1
#define printf          bl_printf
#define scanf           bl_scanf
#define malloc          bl_malloc
#define rand            bl_rand
#define srand           bl_srand
#define gets            bl_gets
#define openWin         bl_openWin
#define setCol          bl_setCol
#define setBCol         bl_setBCol
#define rgb             bl_rgb
#define iCol            bl_iCol
#define flshWin         bl_flshWin
#define getGrpB         bl_getGrpB
#define setPix          bl_setPix
#define fillRect        bl_fillRect
#define drawRect        bl_drawRect
#define drawLine        bl_drawLine
#define rnd             bl_rnd
#define wait            bl_wait
#define color           bl_color
#define locate          bl_locate
#define getPix          bl_getPix
#define waitNF          bl_waitNF
#define inkey           bl_inkey1
#define cls             bl_cls
#define inptInt         bl_inptInt
#define inptFlot        bl_inptFlot
#define setMode         bl_setMode
#define fillOval        bl_fillOval
#define drawStr         bl_drawStr
#define openVWin        bl_openVWin
#define slctWin         bl_slctWin
#define copyRct0        bl_copyRct0
#define copyRct1        bl_copyRct1
#define drawPtrn        bl_drawPtrn_d
#define drawPtrnD       bl_drawPtrn_d
#define drawPtrnR       bl_drawPtrn_r

#if (defined(__cplusplus))
}
#endif

#endif /* __BLIKE_H__ */
