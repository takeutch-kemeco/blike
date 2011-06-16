#include <gtk/gtk.h>
#include "drvgtk_sleep.h"
#include "drvgtk_pthread.h"
#include "drvgtk_transrate_keycode.h"

#include "blikedrv.h"
#include "blike0.h"



extern struct BL_WORK			bl_work;
extern struct DrvGtkPthreadData*	drvgtk_pthread_data; 



static void wait_signal_compliate(gboolean* ready)
{
	while((*ready) == TRUE) {
		g_usleep(0);
	}
}



static gboolean check_and_exit_wt_run_flag(void)
{
	if(drvgtk_pthread_data->wt_run_flag == FALSE) {
		g_thread_exit(NULL);
	}
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
}



void bld_flshWin(int sx, int sy, int x0, int y0)
{
	check_and_exit_wt_run_flag();
	
	drvgtk_pthread_data->signal->flash_window.ready  		= TRUE;
	drvgtk_pthread_data->signal->flash_window.src_frame_buffer	= (gpointer)(bl_work.win[0].buf);
}



void bld_flshSys()
{
	check_and_exit_wt_run_flag();
}



void bld_waitNF()
{
	check_and_exit_wt_run_flag();
	
	drvgtk_msleep(drvgtk_pthread_data->signal_check_interval);
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
	
	return (void*)(g_malloc(bytes));
}

void bld_free(void* p, unsigned int bytes)
{
	check_and_exit_wt_run_flag();
	
	g_free((gpointer)p);
}



extern unsigned char hankaku[4096];
void bld_initFont()
{
	check_and_exit_wt_run_flag();
	
	bl_initFont();
	bl_work.mod |= BL_READYFONTS;
	return;
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
	
	g_mutex_lock(drvgtk_pthread_data->mutex);
}

void bld_unlock()
{
	check_and_exit_wt_run_flag();
	
	g_mutex_unlock(drvgtk_pthread_data->mutex);
}


