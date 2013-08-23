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

#include "drvgtk_pthread.h"
#include "drvgtk_signal_chain.h"

static void resize_window(struct DrvGtkPthreadData *a, gint width, gint height)
{
        resize_MainWindow(a->main_window, width, height);
        show_MainWindow(a->main_window);
}

static void show_window(struct DrvGtkPthreadData *a)
{
        show_MainWindow(a->main_window);
}

static void flash_window(struct DrvGtkPthreadData *a,
                         gpointer src_frame_buffer,
                         gint x,
                         gint y,
                         gint width,
                         gint height)
{
        void round_range(gint *a, gint min, gint max)
        {
                if (*a > max)
                        *a = max;
                else if (*a < min)
                        *a = min;
        }

        round_range(&x, 0, a->main_window->frame_buffer_width - 1);
        round_range(&y, 0, a->main_window->frame_buffer_height - 1);

        round_range(&width, 0, a->main_window->frame_buffer_width - x);
        round_range(&height, 0, a->main_window->frame_buffer_height - y);

        const guint32 src_top_ofst = (y * a->main_window->frame_buffer_width) + x;
        const guint32 dst_top_ofst = src_top_ofst * 3;

        guint32 *src = ((guint32*)src_frame_buffer) + src_top_ofst;
        guchar *dst = ((guchar*)a->main_window->frame_buffer) + dst_top_ofst;

        const guint32 src_next_ofst = a->main_window->frame_buffer_width;
        const guint32 dst_next_ofst = src_next_ofst * 3;

        if(src == NULL)
                return;

        int j = height;
        while (j-->0) {
                guint32 *s = src;
                guchar  *d = dst;
                int i = width;
                while (i-->0) {
                        d[0] = (*s) >> 16;
                        d[1] = (*s) >> 8;
                        d[2] = (*s) >> 0;

                        d += 3;
                        s++;
                }

                src += src_next_ofst;
                dst += dst_next_ofst;
        }

        redraw_MainWindow(a->main_window, x, y, width, height);
}

static void exit_window(struct DrvGtkPthreadData *a)
{
        gtk_main_quit();
}

gboolean update_DrvGtkSignalChain(gpointer data)
{
        struct DrvGtkPthreadData *a = (struct DrvGtkPthreadData*)data;

        (*(a->time_count)) += DRVGTK_SYGNAL_CHECK_INTERVAL;

        if(a->signal->resize_window.ready == TRUE) {
                resize_window(a, a->signal->resize_window.width, a->signal->resize_window.height);
                a->signal->resize_window.ready = FALSE;
        }

        if(a->signal->show_window.ready == TRUE) {
                show_window(a);
                a->signal->show_window.ready = FALSE;
        }

        if(a->signal->flash_window.ready == TRUE) {
                flash_window(a,
                             a->signal->flash_window.src_frame_buffer,
                             a->signal->flash_window.x,
                             a->signal->flash_window.y,
                             a->signal->flash_window.width,
                             a->signal->flash_window.height);
                a->signal->flash_window.ready = FALSE;
        }

        if(a->signal->exit_window.ready == TRUE) {
                exit_window(a);
                a->signal->exit_window.ready == FALSE;
        }

        return TRUE;
}
