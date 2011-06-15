#include <stdlib.h>
#include <pthread.h>
#include "drvgtk_pthread.h"



static void* __pthread_main_window(void* data)
{
	struct DrvGtkPthreadData* a = (struct DrvGtkPthreadData*)data;
	gtk_timeout_add(a->signal_check_interval, a->window_update_program, (gpointer)a);
	gtk_main();

	pthread_cancel(a->ptid);
}

static void pthread_main_window(struct DrvGtkPthreadData* data)
{
	// 別スレッドでgtk_main()を動かすと、なぜか非常に重くなる
	__pthread_main_window(data);
}



static void* __pthread_main_program(void* data)
{
	struct DrvGtkPthreadData* a = (struct DrvGtkPthreadData*)data;

	a->init_control_program();
	a->control_program();
	a->close_control_program();
	
	gtk_main_quit();
	pthread_cancel(a->wtid);
}

static void pthread_main_program(struct DrvGtkPthreadData* data)
{
	pthread_create(&data->ptid, NULL, __pthread_main_program, data);
}



void pthread_main(struct DrvGtkPthreadData* data)
{
	pthread_main_program(data);
	pthread_main_window(data);
	
	pthread_join(data->ptid, NULL);
}


