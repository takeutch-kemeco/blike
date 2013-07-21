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
#include "drvgtk_sleep.h"

static gpointer __pthread_main_window(gpointer data)
{
        volatile struct DrvGtkPthreadData *a = (struct DrvGtkPthreadData*)data;

        if (a->wt_run_flag == FALSE) {
                g_timeout_add(DRVGTK_SYGNAL_CHECK_INTERVAL,
                              a->window_update_program,
                              (gpointer)a);
                a->wt_run_flag = TRUE;
                gtk_main();

                a->wt_run_flag = FALSE;
        }
}

static void pthread_main_window(struct DrvGtkPthreadData *data)
{
        __pthread_main_window(data);
}



static gpointer __pthread_main_program(gpointer data)
{
        volatile struct DrvGtkPthreadData *a = (struct DrvGtkPthreadData*)data;

        while(a->wt_run_flag == FALSE) {
                // スピンロック
        }

        a->init_control_program();
        a->control_program();
        a->close_control_program();

        gtk_main_quit();
}

static void pthread_main_program(struct DrvGtkPthreadData *data)
{
        data->ptid = g_thread_new("pthread_main_program",
                                  __pthread_main_program,
                                  data);
}

void pthread_main(struct DrvGtkPthreadData *data)
{
        pthread_main_program(data);
        pthread_main_window(data);

        g_thread_join(data->ptid);
}
