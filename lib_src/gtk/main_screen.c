#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h> // malloc
#include "main_screen.h"
#include "create_xpm.h"

static const char xpm_name[] = "/tmp/__frame_buffer__.xpm";

static gboolean update_frame_buffer(struct MainScreen* a)
{
	gdk_pixbuf_render_to_drawable(
		a->pixbuf,
		a->wgt->window,
		a->wgt->style->fg_gc[GTK_STATE_NORMAL],
		0, 0, 0, 0,
		a->frame_buffer_width,
		a->frame_buffer_height,
		GDK_RGB_DITHER_NONE, 0, 0);
}



static gboolean expose_event(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
	struct MainScreen* a = (struct MainScreen*)data;
	
	update_frame_buffer(a);
	
	return TRUE;
}

static void init_signal_MainScreen(struct MainScreen* a)
{
	gtk_signal_connect(GTK_OBJECT(a->wgt), "expose_event",		GTK_SIGNAL_FUNC(expose_event),		(gpointer)a);
}



struct MainScreen* new_MainScreen(gint width, gint height, struct MainWindow* window)
{
	struct MainScreen* a = g_malloc(sizeof(*a));
	
	
	a->frame_buffer_width  = width;
	a->frame_buffer_height = height;
	
	create_xpm(xpm_name, a->frame_buffer_width, a->frame_buffer_height);
	a->wgt = gtk_image_new_from_file(xpm_name);

	a->pixbuf = gtk_image_get_pixbuf((GtkImage*)a->wgt);
	a->frame_buffer = gdk_pixbuf_get_pixels(a->pixbuf);
	
	gtk_widget_set_size_request(a->wgt, a->frame_buffer_width, a->frame_buffer_height);
	gtk_widget_set_double_buffered(a->wgt, TRUE);
	gtk_container_add(GTK_CONTAINER(window->wgt), a->wgt);

	
	init_signal_MainScreen(a);
	
	
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
	
	create_xpm(xpm_name, width, height);
	a->wgt = gtk_image_new_from_file(xpm_name);

	gtk_widget_set_size_request(a->wgt, a->frame_buffer_width, a->frame_buffer_height);

	a->pixbuf = gtk_image_get_pixbuf((GtkImage*)a->wgt);
	a->frame_buffer = gdk_pixbuf_get_pixels(a->pixbuf);


	gtk_container_add(GTK_CONTAINER(window->wgt), a->wgt);
	
	gtk_widget_show(a->wgt);
	gtk_widget_show_all(window->wgt);
}


