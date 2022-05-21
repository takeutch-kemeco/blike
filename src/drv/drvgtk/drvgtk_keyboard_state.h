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

#pragma once

#include "drvgtk_key_ring_buffer.h"

#define VK_BACK         0x08
#define VK_TAB          0x09
#define VK_CLEAR        0x0C
#define VK_RETURN       0x0D
#define VK_SHIFT        0x10
#define VK_PAUSE        0x13
#define VK_CONTROL      0x11
#define VK_MENU         0x12    /* Alt */
#define VK_PAUSE        0x13
#define VK_CAPITAL      0x14    /* caps lock */
#define VK_ESCAPE       0x1B
#define VK_CONVERT      0x1C    /* 変換 */
#define VK_NONCONVERT   0x1D    /* 無変換 */
#define VK_SPACE        0x20
#define VK_PRIOR        0x21    /* page up */
#define VK_NEXT         0x22    /* page down */
#define VK_END          0x23
#define VK_HOME         0x24
#define VK_LEFT         0x25
#define VK_UP           0x26
#define VK_RIGHT        0x27
#define VK_DOWN         0x28
#define VK_SNAPSHOT     0x2C    /* print screen */
#define VK_INSERT       0x2D
#define VK_DELETE       0x2E
#define VK_0            0x30
#define VK_1            0x31
#define VK_2            0x32
#define VK_3            0x33
#define VK_4            0x34
#define VK_5            0x35
#define VK_6            0x36
#define VK_7            0x37
#define VK_8            0x38
#define VK_9            0x39
#define VK_A            0x41
#define VK_B            0x42
#define VK_C            0x43
#define VK_D            0x44
#define VK_E            0x45
#define VK_F            0x46
#define VK_G            0x47
#define VK_H            0x48
#define VK_I            0x49
#define VK_J            0x4A
#define VK_K            0x4B
#define VK_L            0x4C
#define VK_M            0x4D
#define VK_N            0x4E
#define VK_O            0x4F
#define VK_P            0x50
#define VK_Q            0x51
#define VK_R            0x52
#define VK_S            0x53
#define VK_T            0x54
#define VK_U            0x55
#define VK_V            0x56
#define VK_W            0x57
#define VK_X            0x58
#define VK_Y            0x59
#define VK_Z            0x5A
#define VK_LWIN         0x5B    /* left windows */
#define VK_RWIN         0x5C    /* right windows */
#define VK_APPS         0x5D    /* popup menu */
#define VK_NUMPAD0      0x60    /* NUMPAD = ten key*/
#define VK_NUMPAD1      0x61
#define VK_NUMPAD2      0x62
#define VK_NUMPAD3      0x63
#define VK_NUMPAD4      0x64
#define VK_NUMPAD5      0x65
#define VK_NUMPAD6      0x66
#define VK_NUMPAD7      0x67
#define VK_NUMPAD8      0x68
#define VK_NUMPAD9      0x69
#define VK_MULTIPLY     0x6A    /* * */
#define VK_ADD          0x6B    /* + */
#define VK_SUBTRACT     0x6D    /* - */
#define VK_DECIMAL      0x6E    /* . */
#define VK_DIVIDE       0x6F    /* / */
#define VK_F1           0x70
#define VK_F2           0x71
#define VK_F3           0x72
#define VK_F4           0x73
#define VK_F5           0x74
#define VK_F6           0x75
#define VK_F7           0x76
#define VK_F8           0x77
#define VK_F9           0x78
#define VK_F10          0x79
#define VK_F11          0x7A
#define VK_F12          0x7B
#define VK_NUMLOCK      0x90
#define VK_SCROLL       0x91
#define VK_LSHIFT       0xA0
#define VK_RSHIFT       0xA1
#define VK_LCONTROL     0xA2
#define VK_RCONTROL     0xA3
#define VK_LMENU        0xA4    /* left Alt */
#define VK_RMENU        0xA5    /* right Alt */
#define VK_OEM_1        0xBA    /* : */
#define VK_OEM_PLUS     0xBB    /* ; */
#define VK_OEM_COMMA    0xBC    /* , */
#define VK_OEM_MINUS    0xBD    /* - */
#define VK_OEM_PERIOD   0xBE    /* . */
#define VK_OEM_2        0xBF    /* / */
#define VK_OEM_3        0xC0    /* @ */
#define VK_OEM_4        0xDB    /* [ */
#define VK_OEM_5        0xDC    /* ￥記号 */
#define VK_OEM_6        0xDD    /* ] */
#define VK_OEM_7        0xDE    /* ^ */
#define VK_OEM_102      0xE2    /* バックスラッシュ */
#define VK_PROCESSKEY   0xE5    /* 全角・半角 */
#define VK_OEM_COPY     0xF2    /* カタカナ・ひらがな */

struct DrvGtkKeybordState {
        unsigned long value[8];
};

struct DrvGtkKeybordState* new_transform_table_DrvGtkKeybordState(void);

void add_DrvGtkKeybordState(struct DrvGtkKeybordState *press,
                            struct DrvGtkKeybordState *release,
                            struct DrvGtkKeybordState *table,
                            struct DrvGtkKey *key);

void next_DrvGtkKeybordState(struct DrvGtkKeybordState *press,
                             struct DrvGtkKeybordState *release);
