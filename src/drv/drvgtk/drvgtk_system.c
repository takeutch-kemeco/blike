#include <gtk/gtk.h>
#include "config.h"

#include "drvgtk_pthread.h"
#include "drvgtk_signal_chain.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keybord_state.h"



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
	gint32*		key_count,
	gint		language
)
{
	struct DrvGtkPthreadData* a 	= g_malloc(sizeof(*a));
	
	a->shared_data 			= shared_data;
	a->time_count			= time_count;
	a->window_update_program	= update_DrvGtkSignalChain;
	a->control_program		= control_program;
	a->init_control_program		= init_control_program;
	a->close_control_program	= close_control_program;
	
	a->signal = new_DrvGtkSignal();
	       
	g_mutex_init(&a->mutex);
	
	a->key_ring_buffer		= new_DrvGtkKeyRingBuffer(key_len, int_key, read_index, write_index, key_count);
	
	a->press			= g_malloc0(sizeof(*(a->press)));
	a->release			= g_malloc0(sizeof(*(a->release)));
	a->key_transform_table		= new_transform_table_DrvGtkKeybordState(language);
	
	
	a->main_window = new_MainWindow(a->key_ring_buffer, a->press, a->release, a->key_transform_table);
	a->main_screen = new_MainScreen(64, 32, a->main_window);
	
	a->wt_run_flag			= FALSE;
	
	return a;
}



void free_DrvGtkPthreadData(struct DrvGtkPthreadData* a)
{
	g_mutex_clear(&a->mutex);

	g_free(a->press);
	g_free(a->release);
	g_free(a->key_transform_table);

	free_DrvGtkKeyRingBuffer(a->key_ring_buffer);
	
	free_DrvGtkSignal(a->signal);
	
	g_free(a);
}



void run_DrvGtkSystem(struct DrvGtkPthreadData* a)
{
	pthread_main(a);
}


