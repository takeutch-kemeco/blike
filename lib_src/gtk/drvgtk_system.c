#include <gtk/gtk.h>
#include "drvgtk_pthread.h"
#include "drvgtk_signal_chain.h"
#include "drvgtk_key_ring_buffer.h"



struct DrvGtkPthreadData* new_DrvGtkPthreadData(
	gpointer	shared_data,
	gint32* 	time_count,
	int 		(*control_program)(),
	void		(*init_control_program)(),
	void		(*close_control_program)(),
	gint32		key_len,
	gint32*		int_key,
	gint32*		read_index,
	gint32*		write_index,
	gint32*		key_count
)
{
	gtk_set_locale();
	gtk_init(NULL, NULL);
	gdk_rgb_init();

	
	struct DrvGtkPthreadData* a 	= g_malloc(sizeof(*a));
	
	a->shared_data 			= shared_data;
	a->time_count			= time_count;
	a->window_update_program	= update_DrvGtkSignalChain;
	a->control_program		= control_program;
	a->init_control_program		= init_control_program;
	a->close_control_program	= close_control_program;
	
	a->signal = new_DrvGtkSignal();
	a->signal_check_interval	= 1000 / 250;	// (250Hz)
	       
	a->mutex			= g_mutex_new();
	
	a->key_ring_buffer		= new_DrvGtkKeyRingBuffer(key_len, int_key, read_index, write_index, key_count);
	
	a->main_window = new_MainWindow(a->key_ring_buffer);
	a->main_screen = new_MainScreen(64, 32, a->main_window);
	
	a->wt_run_flag			= FALSE;
	
	return a;
}



void run_DrvGtkSystem(struct DrvGtkPthreadData* a)
{
	show_MainWindow(a->main_window);
//	hide_MainWindow(a->main_window);
	
	pthread_main(a);
}


