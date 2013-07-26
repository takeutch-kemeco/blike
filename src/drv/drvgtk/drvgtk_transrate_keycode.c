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
#include "drvgtk_keyboard_state.h"
#include "blike0.h"

gint32 __drvgtk_transrate_keycode(struct DrvGtkKey *a)
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

gint32 __drvgtk_transrate_keycode_gtk_to_vk(const gint32 keyval)
{
        switch (keyval) {
        /* 一段目 */
        case GDK_KEY_Escape: return VK_ESCAPE;

        case GDK_KEY_F1: return VK_F1;

        case GDK_KEY_F2: return VK_F2;

        case GDK_KEY_F3: return VK_F3;

        case GDK_KEY_F4: return VK_F4;

        case GDK_KEY_F5: return VK_F5;

        case GDK_KEY_F6: return VK_F6;

        case GDK_KEY_F7: return VK_F7;

        case GDK_KEY_F8: return VK_F8;

        case GDK_KEY_F9: return VK_F9;

        case GDK_KEY_F10: return VK_F10;

        case GDK_KEY_F11: return VK_F11;

        case GDK_KEY_F12: return VK_F12;

        /* 二段目 */
        case GDK_KEY_Zenkaku_Hankaku: return VK_PROCESSKEY;

        case GDK_KEY_exclam: return VK_1;
        case GDK_KEY_1: return VK_1;

        case GDK_KEY_doubleacute: return VK_2;
        case GDK_KEY_2: return VK_2;

        case GDK_KEY_musicalsharp: return VK_3;
        case GDK_KEY_3: return VK_3;

        case GDK_KEY_dollar: return VK_4;
        case GDK_KEY_4: return VK_4;

        case GDK_KEY_percent: return VK_5;
        case GDK_KEY_5: return VK_5;

        case GDK_KEY_ampersand: return VK_6;
        case GDK_KEY_6: return VK_6;

        case GDK_KEY_apostrophe: return VK_7;
        case GDK_KEY_7: return VK_7;

        case GDK_KEY_parenleft: return VK_8;
        case GDK_KEY_8: return VK_8;

        case GDK_KEY_parenright: return VK_9;
        case GDK_KEY_9: return VK_9;

        case GDK_KEY_0: return VK_0;

        case GDK_KEY_equal: return VK_OEM_MINUS;
        case GDK_KEY_minus: return VK_OEM_MINUS;

        case GDK_KEY_asciitilde: return VK_OEM_7;
        case GDK_KEY_dead_circumflex: return VK_OEM_7;

        case GDK_KEY_bar: return VK_OEM_5;
        case GDK_KEY_backslash: return VK_OEM_5;

        case GDK_KEY_BackSpace: return VK_BACK;

        /* 三段目 */
        case GDK_KEY_Tab: return VK_TAB;

        case GDK_KEY_Q: return VK_Q;
        case GDK_KEY_q: return VK_Q;

        case GDK_KEY_W: return VK_W;
        case GDK_KEY_w: return VK_W;

        case GDK_KEY_E: return VK_E;
        case GDK_KEY_e: return VK_E;

        case GDK_KEY_R: return VK_R;
        case GDK_KEY_r: return VK_R;

        case GDK_KEY_T: return VK_T;
        case GDK_KEY_t: return VK_T;

        case GDK_KEY_Y: return VK_Y;
        case GDK_KEY_y: return VK_Y;

        case GDK_KEY_U: return VK_U;
        case GDK_KEY_u: return VK_U;

        case GDK_KEY_I: return VK_I;
        case GDK_KEY_i: return VK_I;

        case GDK_KEY_O: return VK_O;
        case GDK_KEY_o: return VK_O;

        case GDK_KEY_P: return VK_P;
        case GDK_KEY_p: return VK_P;

        case GDK_KEY_at: return VK_OEM_3;
        case GDK_KEY_grave: return VK_OEM_3;

        case GDK_KEY_braceleft: return VK_OEM_4;
        case GDK_KEY_bracketleft: return VK_OEM_4;

        case GDK_KEY_Return: return VK_RETURN;

        /* 四段目 */
        case GDK_KEY_Caps_Lock: return VK_CAPITAL;

        case GDK_KEY_A: return VK_A;
        case GDK_KEY_a: return VK_A;

        case GDK_KEY_S: return VK_S;
        case GDK_KEY_s: return VK_S;

        case GDK_KEY_D: return VK_D;
        case GDK_KEY_d: return VK_D;

        case GDK_KEY_F: return VK_F;
        case GDK_KEY_f: return VK_F;

        case GDK_KEY_G: return VK_G;
        case GDK_KEY_g: return VK_G;

        case GDK_KEY_H: return VK_H;
        case GDK_KEY_h: return VK_H;

        case GDK_KEY_J: return VK_J;
        case GDK_KEY_j: return VK_J;

        case GDK_KEY_K: return VK_K;
        case GDK_KEY_k: return VK_K;

        case GDK_KEY_L: return VK_L;
        case GDK_KEY_l: return VK_L;

        case GDK_KEY_plus: return VK_OEM_PLUS;
        case GDK_KEY_semicolon: return VK_OEM_PLUS;

        case GDK_KEY_asterisk: return VK_OEM_1;
        case GDK_KEY_colon: return VK_OEM_1;

        case GDK_KEY_braceright: return VK_OEM_6;
        case GDK_KEY_bracketright: return VK_OEM_6;

        /* 五段目 */
        case GDK_KEY_Shift_L: return VK_LSHIFT;

        case GDK_KEY_Z: return VK_Z;
        case GDK_KEY_z: return VK_Z;

        case GDK_KEY_X: return VK_X;
        case GDK_KEY_x: return VK_X;

        case GDK_KEY_C: return VK_C;
        case GDK_KEY_c: return VK_C;

        case GDK_KEY_V: return VK_V;
        case GDK_KEY_v: return VK_V;

        case GDK_KEY_B: return VK_B;
        case GDK_KEY_b: return VK_B;

        case GDK_KEY_N: return VK_N;
        case GDK_KEY_n: return VK_N;

        case GDK_KEY_M: return VK_M;
        case GDK_KEY_m: return VK_M;

        case GDK_KEY_less: return VK_OEM_COMMA;
        case GDK_KEY_comma: return VK_OEM_COMMA;

        case GDK_KEY_greater: return VK_OEM_PERIOD;
        case GDK_KEY_period: return VK_OEM_PERIOD;

        case GDK_KEY_question: return VK_OEM_2;
        case GDK_KEY_slash: return VK_OEM_2;

        case GDK_KEY_underscore: return VK_OEM_102;
        /* case GDK_KEY_backslash: return VK_OEM_102; */

        case GDK_KEY_Shift_R: return VK_RSHIFT;

        /* 六段目 */
        case GDK_KEY_Control_L: return VK_LCONTROL;

        /* : return VK_LWIN; */

        case GDK_KEY_Alt_L: return VK_LMENU;

        case GDK_KEY_Muhenkan: return VK_NONCONVERT;

        case GDK_KEY_space: return VK_SPACE;

        case GDK_KEY_Mae_Koho: return VK_PROCESSKEY;

        case GDK_KEY_Hiragana_Katakana: return VK_OEM_COPY;

        case GDK_KEY_Alt_R: return VK_RMENU;

        /* : return VK_RWIN; */

        /* : return VK_APPS; */

        case GDK_KEY_Control_R: return VK_RCONTROL;

        /* 右側 */
        case GDK_KEY_Print: return VK_SNAPSHOT;

        case GDK_KEY_Scroll_Lock: return VK_SCROLL;

        case GDK_KEY_Pause: return VK_PAUSE;

        case GDK_KEY_Insert: return VK_INSERT;

        case GDK_KEY_Home: return VK_HOME;

        case GDK_KEY_Page_Up: return VK_PRIOR;

        case GDK_KEY_Delete: return VK_DELETE;

        case GDK_KEY_End: return VK_END;

        case GDK_KEY_Next: return VK_NEXT;

        case GDK_KEY_Up: return VK_UP;

        case GDK_KEY_Left: return VK_LEFT;

        case GDK_KEY_Down: return VK_DOWN;

        case GDK_KEY_Right: return VK_RIGHT;

        /* テンキー */
        case GDK_KEY_Num_Lock: return VK_NUMLOCK;

        case GDK_KEY_KP_Divide: return VK_DIVIDE;

        case GDK_KEY_KP_Multiply: return VK_MULTIPLY;

        case GDK_KEY_KP_Subtract: return VK_SUBTRACT;

        case GDK_KEY_KP_7: return VK_NUMPAD7;

        case GDK_KEY_KP_8: return VK_NUMPAD8;

        case GDK_KEY_KP_9: return VK_NUMPAD9;

        case GDK_KEY_KP_Add: return VK_ADD;

        case GDK_KEY_KP_4: return VK_NUMPAD4;

        case GDK_KEY_KP_5: return VK_NUMPAD5;

        case GDK_KEY_KP_6: return VK_NUMPAD6;

        case GDK_KEY_KP_1: return VK_NUMPAD1;

        case GDK_KEY_KP_2: return VK_NUMPAD2;

        case GDK_KEY_KP_3: return VK_NUMPAD3;

        /* case GDK_KEY_Return: return VK_RETURN; */

        case GDK_KEY_KP_0: return VK_NUMPAD0;

        case GDK_KEY_KP_Decimal: return VK_DECIMAL;
        }

        return -1;
}
