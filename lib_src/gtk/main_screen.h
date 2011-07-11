#include <gtk/gtk.h>
#include "main_window.h"

#ifndef __MAIN_SCREEN_H__
#define __MAIN_SCREEN_H__

struct MainScreen {
	GtkWidget*	wgt;
	GdkPixbuf*	pixbuf;
	
	guchar*		frame_buffer;
	gint		frame_buffer_width;
	gint		frame_buffer_height;
} __attribute__((aligned(16)));

extern struct MainScreen* new_MainScreen(gint width, gint height, struct MainWindow* window);

extern void redraw_MainScreen(struct MainScreen* a);
extern void resize_MainScreen(struct MainScreen* a, gint width, gint height, struct MainWindow* window);

#endif //__MAIN_SCREEN_H__
