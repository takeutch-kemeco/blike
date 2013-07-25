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

#include <gtk/gtk.h>
#include "config.h"

#include "drvgtk_pthread.h"
#include "drvgtk_signal_chain.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"

struct DrvGtkPthreadData*
new_DrvGtkPthreadData(gpointer shared_data,
                      gint32 *time_count,
                      int (*control_program)(),
                      void (*init_control_program)(),
                      void (*close_control_program)(),
                      gint32 key_len,
                      gint32 *int_key,
                      gint32 *read_index,
                      gint32 *write_index,
                      gint32 *key_count,
                      DrvGtkFuncPutKeyBuffer func_put_key_buffer)
{
        struct DrvGtkPthreadData *a     = g_malloc(sizeof(*a));

        a->shared_data                  = shared_data;
        a->time_count                   = time_count;
        a->window_update_program        = update_DrvGtkSignalChain;
        a->control_program              = control_program;
        a->init_control_program         = init_control_program;
        a->close_control_program        = close_control_program;

        a->signal = new_DrvGtkSignal();

        g_mutex_init(&a->mutex);

        a->key_ring_buffer =
                new_DrvGtkKeyRingBuffer(key_len,
                                        int_key,
                                        read_index,
                                        write_index,
                                        key_count,
                                        func_put_key_buffer);

        a->press                        = g_malloc0(sizeof(*(a->press)));
        a->release                      = g_malloc0(sizeof(*(a->release)));
        a->key_transform_table          = new_transform_table_DrvGtkKeybordState();

        a->main_window = new_MainWindow(a->key_ring_buffer, a->press, a->release, a->key_transform_table);

        a->wt_run_flag                  = FALSE;

        return a;
}

void free_DrvGtkPthreadData(struct DrvGtkPthreadData *a)
{
        g_mutex_clear(&a->mutex);

        g_free(a->press);
        g_free(a->release);
        g_free(a->key_transform_table);

        free_DrvGtkKeyRingBuffer(a->key_ring_buffer);

        free_DrvGtkSignal(a->signal);

        g_free(a);
}

void run_DrvGtkSystem(struct DrvGtkPthreadData *a)
{
        pthread_main(a);
}
