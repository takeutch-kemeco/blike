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

#include <gtk/gtk.h>
#include <glib.h>
#include "main_window.h"
#include "drvgtk_signal.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"
#include "blikedrv.h"

#define DRVGTK_SYGNAL_CHECK_INTERVAL_MS 1 // 1ms (1000Hz)

struct DrvGtkPthreadData {
        GtkApplication *app;

        struct MainWindow *main_window;

        struct BL_WORK *bl_work;
        gint32 *time_count;

        struct DrvGtkSignal *signal;

        GMutex mutex;

        int (*control_program)();

        GThread *ptid;

        struct DrvGtkKeyRingBuffer *key_ring_buffer;

        struct DrvGtkKeybordState *press;
        struct DrvGtkKeybordState *release;
        struct DrvGtkKeybordState *key_transform_table;
};

extern struct DrvGtkPthreadData* drvgtk_pthread_data;

struct DrvGtkPthreadData*
new_DrvGtkPthreadData(struct BL_WORK *bl_work,
                      gint32 *time_count,
                      int (*control_program)(),
                      gint32 key_len,
                      gint32 *int_key,
                      gint32 *read_index,
                      gint32 *write_index,
                      gint32 *key_count,
                      DrvGtkFuncPutKeyBuffer func_put_key_buffer);
void free_DrvGtkPthreadData(struct DrvGtkPthreadData *a);
void run_DrvGtkSystem(struct DrvGtkPthreadData *a);
