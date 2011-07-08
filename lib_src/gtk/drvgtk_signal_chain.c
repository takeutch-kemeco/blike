#include <glib.h>
#include "drvgtk_pthread.h"
#include "drvgtk_signal_chain.h"



static void resize_window(struct DrvGtkPthreadData* a, gint width, gint height)
{
	resize_MainWindow(a->main_window, width, height);
	resize_MainScreen(a->main_screen, width, height, a->main_window);

	show_MainWindow(a->main_window);
	redraw_MainScreen(a->main_screen);
}



static void show_window(struct DrvGtkPthreadData* a)
{
	show_MainWindow(a->main_window);
}



static void flash_window(struct DrvGtkPthreadData* a, gpointer src_frame_buffer)
{
	guint32* p = (guint32*)src_frame_buffer;
	guchar*  q = a->main_screen->frame_buffer;

	gint i = a->main_screen->frame_buffer_width * a->main_screen->frame_buffer_height;
	if(p == NULL) {
		while(i-->0){
			*q++ = rand()%0xFF;
			*q++ = rand()%0xFF;
			*q++ = rand()%0xFF;
			
			p++;
		}
	}
	else {
		while(i-->0){
			*q++ = ((*p)>>16) & 0xFF;
			*q++ = ((*p)>>8 ) & 0xFF;
			*q++ = ((*p)>>0 ) & 0xFF;
			
			p++;
		}
	}

	redraw_MainScreen(a->main_screen);
}



static void exit_window(struct DrvGtkPthreadData* a)
{
	gtk_main_quit();
}



gboolean update_DrvGtkSignalChain(gpointer data)
{
	struct DrvGtkPthreadData* a = (struct DrvGtkPthreadData*)data;
	

	(*(a->time_count)) += a->signal_check_interval;
	
	
	if(a->signal->resize_window.ready == TRUE) {
		resize_window(a, a->signal->resize_window.width, a->signal->resize_window.height);
		a->signal->resize_window.ready = FALSE;
	}

	if(a->signal->show_window.ready == TRUE) {
		show_window(a);
		a->signal->show_window.ready = FALSE;
	}
	
	if(a->signal->flash_window.ready == TRUE) {
		flash_window(a, a->signal->flash_window.src_frame_buffer);
		a->signal->flash_window.ready = FALSE;
	}
	
	if(a->signal->exit_window.ready == TRUE) {
		exit_window(a);
		a->signal->exit_window.ready == FALSE;
	}
	
	
	return TRUE;
}


