#include <gtk/gtk.h>
#include "drvgtk_pthread.h"
#include "drvgtk_signal_chain.h"



struct DrvGtkPthreadData* new_DrvGtkPthreadData(
	gpointer	shared_data,
	gint32* 	time_count,
	int 		(*control_program)()
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
	
	a->signal = new_DrvGtkSignal();
	a->signal_check_interval	= 1000 / 250;	// (250Hz)

	
	a->main_window = new_MainWindow();
	a->main_screen = new_MainScreen(64, 32, a->main_window);
	
	a->key_ring_buffer		= a->main_window->key_ring_buffer;
	
	return a;
}



void run_DrvGtkSystem(struct DrvGtkPthreadData* a)
{
	show_MainWindow(a->main_window);
//	hide_MainWindow(a->main_window);
	
	pthread_main(a);
}


