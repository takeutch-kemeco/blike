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
#include "config.h"

#include "drvgtk_system.h"
#include "drvgtk_signal.h"

struct DrvGtkSignal* new_DrvGtkSignal(void)
{
        struct DrvGtkSignal *a = g_malloc(sizeof(*a));

        a->resize_window.ready = FALSE;
        a->show_window.ready   = FALSE;
        a->flash_window.ready  = FALSE;
        a->exit_window.ready   = FALSE;

        return a;
}

void free_DrvGtkSignal(struct DrvGtkSignal *a)
{
        g_free(a);
}

gboolean update_DrvGtkSignalChain(gpointer data)
{
        struct DrvGtkPthreadData *a = (struct DrvGtkPthreadData*)data;
        struct MainWindow *m = a->main_window;

        (*(a->time_count)) += DRVGTK_SYGNAL_CHECK_INTERVAL_MS;

        if(a->signal->resize_window.ready == TRUE) {
                struct DrvGtkSignal_resize_window *s = &a->signal->resize_window;
                g_signal_emit_by_name(m->wgt, "drvgtk-resize-window",s->width, s->height);
                a->signal->resize_window.ready = FALSE;
        }

        if(a->signal->show_window.ready == TRUE) {
                g_signal_emit_by_name(m->wgt, "drvgtk-show-window");
                a->signal->show_window.ready = FALSE;
        }

        if(a->signal->flash_window.ready == TRUE) {
                struct DrvGtkSignal_flash_window *s = &a->signal->flash_window;
                g_signal_emit_by_name(m->wgt, "drvgtk-flash-window", s->x, s->y, s->width, s->height);
                a->signal->flash_window.ready = FALSE;
        }

        if(a->signal->exit_window.ready == TRUE) {
                g_signal_emit_by_name(m->wgt, "drvgtk-exit-window");
                a->signal->exit_window.ready = FALSE;
        }

        return TRUE;
}
