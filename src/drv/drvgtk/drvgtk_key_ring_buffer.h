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

#ifndef __DRVGTK_KEY_RING_BUFFER_H__
#define __DRVGTK_KEY_RING_BUFFER_H__

enum DrvGtkKeyState {
        DrvGtkKeyState_press   = 1,
        DrvGtkKeyState_release = 2,
        DrvGtkKeyState_none    = 0,
};

struct DrvGtkKey {
        guint32 value;
        enum DrvGtkKeyState state;
};

typedef void (*DrvGtkFuncPutKeyBuffer)(int, int*);

struct DrvGtkKeyRingBuffer {
        struct DrvGtkKey *key;
        gint32 key_len;
        gint32 *int_key;
        gint32 *read_index;
        gint32 *write_index;
        gint32 *key_count;
        DrvGtkFuncPutKeyBuffer func_put_key_buffer;
};

struct DrvGtkKeyRingBuffer*
new_DrvGtkKeyRingBuffer(gint32 key_len,
                        gint32 *int_key,
                        gint32 *read_index,
                        gint32 *write_index,
                        gint32 *key_count,
                        DrvGtkFuncPutKeyBuffer func_put_key_buffer);
void free_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer *a);
void write_c_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer *a,
                                 struct DrvGtkKey *key);

#endif /* __DRVGTK_KEY_RING_BUFFER_H__ */
