#include <gtk/gtk.h>
#include "config.h"

#include "drvgtk_sleep.h"
#include "drvgtk_pthread.h"
#include "drvgtk_transrate_keycode.h"
#include "drvgtk_keybord_state.h"
#include "drvgtk_malloc.h"

#include "blikedrv.h"
#include "blike0.h"

extern struct BL_WORK __attribute__((aligned(16))) bl_work;

extern struct DrvGtkPthreadData* drvgtk_pthread_data;

static void wait_signal_compliate(volatile gboolean *ready)
{
        while((*ready) == TRUE) {
                /* スピンロック */
        }
}

static gboolean check_and_exit_wt_run_flag(void)
{
        if(drvgtk_pthread_data->wt_run_flag == FALSE)
                g_thread_exit(NULL);
}

static void bld_showWin(void)
{
        drvgtk_pthread_data->signal->show_window.ready = TRUE;

        wait_signal_compliate(&(drvgtk_pthread_data->signal->show_window.ready));
}

void bld_openWin(int x, int y)
{
        check_and_exit_wt_run_flag();

        drvgtk_pthread_data->signal->resize_window.ready  = TRUE;
        drvgtk_pthread_data->signal->resize_window.width  = x;
        drvgtk_pthread_data->signal->resize_window.height = y;

        wait_signal_compliate(&(drvgtk_pthread_data->signal->resize_window.ready));

        bld_flshWin(0, 0, x, y);
}

void bld_flshWin(int sx, int sy, int x0, int y0)
{
        check_and_exit_wt_run_flag();

        drvgtk_pthread_data->signal->flash_window.ready = TRUE;
        drvgtk_pthread_data->signal->flash_window.src_frame_buffer = (gpointer)(bl_work.win[0].buf);
        drvgtk_pthread_data->signal->flash_window.x      = x0;
        drvgtk_pthread_data->signal->flash_window.y      = y0;
        drvgtk_pthread_data->signal->flash_window.width  = sx;
        drvgtk_pthread_data->signal->flash_window.height = sy;

        wait_signal_compliate(&(drvgtk_pthread_data->signal->flash_window.ready));
}

void bld_flshSys()
{
        check_and_exit_wt_run_flag();
}

void bld_waitNF()
{
        check_and_exit_wt_run_flag();

        drvgtk_msleep(DRVGTK_SYGNAL_CHECK_INTERVAL);
}

void bld_exit()
{
        check_and_exit_wt_run_flag();

        drvgtk_pthread_data->signal->exit_window.ready = TRUE;

        wait_signal_compliate(&(drvgtk_pthread_data->signal->exit_window.ready));
}

int bld_getSeed()
{
        check_and_exit_wt_run_flag();

        GTimeVal a;
        g_get_current_time(&a);
        return (int)(a.tv_sec);
}

void* bld_malloc(unsigned int bytes)
{
        check_and_exit_wt_run_flag();

        return (void*)(drvgtk_malloc_aligned16(bytes));
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

        g_vsnprintf(b, n, f, ap);
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

void __bld_get_keybord_state(unsigned long *press, unsigned long *release)
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
