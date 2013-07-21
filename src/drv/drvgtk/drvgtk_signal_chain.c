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

static inline void flash_line(guchar *dst, guint32 *src, gint i)
{
        while (i-->0) {
                *dst++ = ((*src)>>16) & 0xFF;
                *dst++ = ((*src)>>8 ) & 0xFF;
                *dst++ = ((*src)>>0 ) & 0xFF;

                src++;
        }
}

static void flash_window_whole_area(struct DrvGtkPthreadData *a,
                                    gpointer src_frame_buffer)
{
        guint32 *src = (guint32*)src_frame_buffer;
        guchar *dst = (guchar*)a->main_window->frame_buffer;

        const guint area_len =
                a->main_window->frame_buffer_width * a->main_window->frame_buffer_height;

        if(src != NULL)
                flash_line(dst, src, area_len);
}

static void flash_window_part_area(struct DrvGtkPthreadData *a,
                                   gpointer src_frame_buffer,
                                   const gint x,
                                   const gint y,
                                   const gint width,
                                   const gint height)
{
        const guint32 src_top_ofst = (y * a->main_window->frame_buffer_width) + x;
        const guint32 dst_top_ofst = src_top_ofst * 3;

        guint32 *src = ((guint32*)src_frame_buffer) + src_top_ofst;
        guchar *dst = ((guchar*)a->main_window->frame_buffer) + dst_top_ofst;

        const guint32 src_next_ofst = a->main_window->frame_buffer_width;
        const guint32 dst_next_ofst = src_next_ofst * 3;

        const guint line_len = width;

        if(src == NULL)
                return;

        int j;
        for (j = 0; j < height; j++) {
                flash_line(dst, src, line_len);

                src += src_next_ofst;
                dst += dst_next_ofst;
        }
}

static inline void round_range(gint *a, const gint min, const gint max)
{
        if (*a > max)
                *a = max;
        else if (*a < min)
                *a = min;
}

static void flash_window(struct DrvGtkPthreadData *a,
                         gpointer src_frame_buffer,
                         gint x,
                         gint y,
                         gint width,
                         gint height)
{
        round_range(&x, 0, a->main_window->frame_buffer_width - 1);
        round_range(&y, 0, a->main_window->frame_buffer_height - 1);

        round_range(&width, 0, a->main_window->frame_buffer_width - x);
        round_range(&height, 0, a->main_window->frame_buffer_height - y);

        if (x == 0 &&
            y == 0 &&
            width == a->main_window->frame_buffer_width &&
            height == a->main_window->frame_buffer_height)
                flash_window_whole_area(a, src_frame_buffer);
        else
                flash_window_part_area(a, src_frame_buffer, x, y, width, height);

        redraw_MainWindow(a->main_window);
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
