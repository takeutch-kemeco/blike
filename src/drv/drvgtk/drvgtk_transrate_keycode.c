/* c_blike_01f_linux -
 * Copyright (C) 2011, 2012, 2013 Kemeco Takeutch <takeutchkemeco@gmail.com>
 * All rights reserved.
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

#include <glib.h>
#include <gdk/gdkkeysyms.h>
#include "config.h"

#include "drvgtk_key_ring_buffer.h"
#include <blike0.h>

gint32 transrate_keycode_DrvGtkKey(struct DrvGtkKey *a)
{
        // 入力が無いときは０を返す
        if (a->state == GDK_KEY_VoidSymbol)
                return 0;

        gint32 k;

        // GtkのキーコードをBlikeのキーコードへ変換
        // 該当しないキーコードはそのまま返す
        switch(a->value) {
        case GDK_KEY_Return:            k = KEY_ENTER;          break;
        case GDK_KEY_Escape:            k = KEY_ESC;            break;
        case GDK_KEY_BackSpace:         k = KEY_BACKSPACE;      break;
        case GDK_KEY_Tab:               k = KEY_TAB;            break;
        case GDK_KEY_Left:              k = KEY_LEFT;           break;
        case GDK_KEY_Right:             k = KEY_RIGHT;          break;
        case GDK_KEY_Up:                k = KEY_UP;             break;
        case GDK_KEY_Down:              k = KEY_DOWN;           break;
        case GDK_KEY_Insert:            k = KEY_INS;            break;
        case GDK_KEY_Delete:            k = KEY_DEL;            break;
        default:                        k = a->value;           break;
        }

        // キーを離した瞬間は 4095 を返す
        if (a->state == DrvGtkKeyState_release)
                k = 0x0FFF;

        return k;
}
