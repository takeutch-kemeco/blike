#include <glib.h>
#include "config.h"

#include "drvgtk_pthread.h"
#include "drvgtk_sleep.h"



static gpointer __pthread_main_window(gpointer data)
{
	volatile struct DrvGtkPthreadData* a = (struct DrvGtkPthreadData*)data;
	
	if(a->wt_run_flag == FALSE) {
		g_timeout_add(
			DRVGTK_SYGNAL_CHECK_INTERVAL,
			a->window_update_program,
			(gpointer)a
		);
		a->wt_run_flag = TRUE;
		gtk_main();
		
		a->wt_run_flag = FALSE;
	}
}

static void pthread_main_window(struct DrvGtkPthreadData* data)
{
	// 別スレッドでgtk_main()を動かすと、なぜか非常に重くなる
	__pthread_main_window(data);
}



static gpointer __pthread_main_program(gpointer data)
{
	volatile struct DrvGtkPthreadData* a = (struct DrvGtkPthreadData*)data;
	
	while(a->wt_run_flag == FALSE) {
	}
	
	a->init_control_program();
	a->control_program();
	a->close_control_program();
	
	gtk_main_quit();
}

static void pthread_main_program(struct DrvGtkPthreadData* data)
{
	data->ptid = g_thread_create(__pthread_main_program, data, TRUE, NULL);
}



void pthread_main(struct DrvGtkPthreadData* data)
{
	pthread_main_program(data);
	pthread_main_window(data);
	
	g_thread_join(data->ptid);
}


