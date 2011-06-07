#include <stdio.h>

#include <pthread.h>

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
	}
}



static void bld_showWin(void)
{
	drvgtk_pthread_data->signal->show_window.ready = TRUE;

	wait_signal_compliate(&(drvgtk_pthread_data->signal->show_window.ready));	
}



void bld_openWin(int x, int y)
{
	drvgtk_pthread_data->signal->resize_window.ready  = TRUE;
	drvgtk_pthread_data->signal->resize_window.width  = x;
	drvgtk_pthread_data->signal->resize_window.height = y;

	wait_signal_compliate(&(drvgtk_pthread_data->signal->resize_window.ready));	
}



void bld_flshWin(int sx, int sy, int x0, int y0)
{
	drvgtk_pthread_data->signal->flash_window.ready  		= TRUE;
	drvgtk_pthread_data->signal->flash_window.src_frame_buffer	= (gpointer)(bl_work.win[0].buf);
}



void bld_flshSys()
{
}



void bld_waitNF()
{
	drvgtk_msleep(drvgtk_pthread_data->signal_check_interval);
}



void bld_exit()
{
	drvgtk_pthread_data->signal->exit_window.ready = TRUE;

	wait_signal_compliate(&(drvgtk_pthread_data->signal->exit_window.ready));
}






int bld_getSeed()
{
	GTimeVal a;
	g_get_current_time(&a);
	return (int)(a.tv_sec);
}



void* bld_malloc(unsigned int bytes)
{
	return (void*)(g_malloc(bytes));
}

void bld_free(void* p, unsigned int bytes)
{
	g_free((gpointer)p);
}



extern unsigned char hankaku[4096];
void bld_initFont()
{
	bl_initFont();
	bl_work.mod |= BL_READYFONTS;
	return;
}

int bld_maxfonts()
{
	return 65536 * 4;
}

int bld_vsnprintf(char *b, int n, const char *f, va_list ap)
{
	g_vsnprintf(b, n, f, ap);
}



static pthread_mutex_t mutex = PTHREAD_MUTEX_INITIALIZER;

void bld_lock()
{
	pthread_mutex_lock(&mutex);
}

void bld_unlock()
{
	pthread_mutex_unlock(&mutex);
}


