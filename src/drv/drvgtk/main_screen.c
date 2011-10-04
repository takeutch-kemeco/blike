#include <gtk/gtk.h>
#include "drvgtk_build_flag.h"

#include "main_screen.h"

struct MainScreen* new_MainScreen(gint width, gint height, struct MainWindow* window)
{
	struct MainScreen* a = g_malloc(sizeof(*a));
	
	
	a->frame_buffer_width  = width;
	a->frame_buffer_height = height;
	
	a->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
	a->wgt = gtk_image_new_from_pixbuf(a->pixbuf);

	a->frame_buffer = gdk_pixbuf_get_pixels(a->pixbuf);
	
	gtk_widget_set_size_request(a->wgt, a->frame_buffer_width, a->frame_buffer_height);
	gtk_widget_set_double_buffered(a->wgt, TRUE);
	gtk_container_add(GTK_CONTAINER(window->wgt), a->wgt);


	return a;
}

void redraw_MainScreen(struct MainScreen* a)
{
	gtk_widget_hide(a->wgt);
	gtk_widget_show_now(a->wgt);
}

void resize_MainScreen(struct MainScreen* a, gint width, gint height, struct MainWindow* window)
{
	gtk_container_remove(GTK_CONTAINER(window->wgt), a->wgt);

	
	a->frame_buffer_width  = width;
	a->frame_buffer_height = height;
	
	a->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
	a->wgt = gtk_image_new_from_pixbuf(a->pixbuf);

	gtk_widget_set_size_request(a->wgt, a->frame_buffer_width, a->frame_buffer_height);

	a->frame_buffer = gdk_pixbuf_get_pixels(a->pixbuf);

	gtk_container_add(GTK_CONTAINER(window->wgt), a->wgt);
	

	gtk_widget_show(a->wgt);
}

