/* c_blike_01f_linux -
 * Copyright (C) 2011, 2012, 2013 Kemeco Takeutch <takeutchkemeco@gmail.com>
 * All rights reserved.
 *
 * c_blike_01f -
 * Copyright (C) 2011 H.Kawai (under KL-01)
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

#include <stdarg.h>
#include <gtk/gtk.h>
#include "config.h"

#include "main_window.h"
#include "drvgtk_sleep.h"
#include "drvgtk_pthread.h"
#include "drvgtk_translate_keycode.h"
#include "drvgtk_malloc.h"
#include "blikedrv.h"
#include "blike0.h"

extern struct BL_WORK __attribute__((aligned(16))) bl_work;

extern struct DrvGtkPthreadData* drvgtk_pthread_data;

static void check_and_exit_wt_run_flag(void)
{
        if (g_application_get_is_registered(G_APPLICATION(drvgtk_pthread_data->app)) == FALSE)
                g_thread_exit(NULL);
}

static void bld_showWin(void)
{
        drvgtk_pthread_data->signal->show_window.ready = TRUE;
}

void bld_openWin(int x, int y)
{
        check_and_exit_wt_run_flag();

        drvgtk_pthread_data->signal->resize_window.width  = x;
        drvgtk_pthread_data->signal->resize_window.height = y;

        drvgtk_pthread_data->signal->resize_window.ready = TRUE;

        bld_flshWin(0, 0, x, y);
}

void bld_flshWin(int sx, int sy, int x0, int y0)
{
        check_and_exit_wt_run_flag();

        drvgtk_pthread_data->signal->flash_window.src_frame_buffer = (gpointer)(bl_work.win[0].buf);
        drvgtk_pthread_data->signal->flash_window.x      = x0;
        drvgtk_pthread_data->signal->flash_window.y      = y0;
        drvgtk_pthread_data->signal->flash_window.width  = sx;
        drvgtk_pthread_data->signal->flash_window.height = sy;

        drvgtk_pthread_data->signal->flash_window.ready = TRUE;
}

void bld_flshSys()
{
        check_and_exit_wt_run_flag();
}

void bld_waitNF()
{
        check_and_exit_wt_run_flag();

        drvgtk_msleep(DRVGTK_SYGNAL_CHECK_INTERVAL_MS);
}

void bld_exit()
{
        check_and_exit_wt_run_flag();

        drvgtk_pthread_data->signal->exit_window.ready = TRUE;
}

// 乱数シード値を得るための関数
int bld_getSeed()
{
        GRand *a = g_rand_new(); // 乱数のインスタンスを作成
        guint32 x = g_rand_int(a); // 乱数を得る
        g_rand_free(a); // インスタンスを開放
        return x;
}

void* bld_malloc(unsigned int bytes)
{
        check_and_exit_wt_run_flag();

        return (void*)drvgtk_malloc_aligned16(bytes);
}

void* __bld_mallocRWE(unsigned int bytes)
{
        return (void*)drvgtk_malloc_rwe(bytes);
}

void bld_free(void* p, unsigned int bytes)
{
        check_and_exit_wt_run_flag();

        drvgtk_free_aligned16((gpointer)p);
}

extern unsigned char hankaku[4096];

void bld_initFont()
{
        check_and_exit_wt_run_flag();

        bl_initFont();
        bl_work.mod |= BL_READYFONTS;
}

int bld_maxfonts()
{
        check_and_exit_wt_run_flag();

        return 65536 * 4;
}

int bld_vsnprintf(char *b, int n, const char *f, va_list ap)
{
        check_and_exit_wt_run_flag();

        return vsnprintf(b, n, f, ap);
}

void bld_lock()
{
        check_and_exit_wt_run_flag();

        g_mutex_lock(&drvgtk_pthread_data->mutex);
}

void bld_unlock()
{
        check_and_exit_wt_run_flag();

        g_mutex_unlock(&drvgtk_pthread_data->mutex);
}

void __bld_get_keyboard_state(unsigned long *press, unsigned long *release)
{
        check_and_exit_wt_run_flag();

        g_mutex_lock(&drvgtk_pthread_data->mutex);

        gint i = 8;
        while (i-->0) {
                press[i] = drvgtk_pthread_data->press->value[i];
                release[i] = drvgtk_pthread_data->release->value[i];
        }

        next_DrvGtkKeybordState(drvgtk_pthread_data->press, drvgtk_pthread_data->release);

        g_mutex_unlock(&drvgtk_pthread_data->mutex);
}

void __bld_set_callback_arg(void* a)
{
        drvgtk_pthread_data->main_window->callback_arg = a;
}

void __bld_set_callback_motion_notify(bld_callback_motion_notify_MainWindow func)
{
        drvgtk_pthread_data->main_window->callback_motion_notify = func;
}

void __bld_set_callback_button_press(bld_callback_button_press_MainWindow func)
{
        drvgtk_pthread_data->main_window->callback_button_press = func;
}

void __bld_set_callback_button_release(bld_callback_button_release_MainWindow func)
{
        drvgtk_pthread_data->main_window->callback_button_release = func;
}

void __bld_set_callback_key_press(bld_callback_key_press_MainWindow func)
{
        drvgtk_pthread_data->main_window->callback_key_press = func;
}

void __bld_set_callback_key_release(bld_callback_key_release_MainWindow func)
{
        drvgtk_pthread_data->main_window->callback_key_release = func;
}
