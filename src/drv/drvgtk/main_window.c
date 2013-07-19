#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "config.h"

#include "main_window.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"

static gboolean press_key_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;
        GdkEventKey *key = (GdkEventKey*)event;

        struct DrvGtkKey tmp;
        tmp.state = DrvGtkKeyState_press;
        tmp.value = key->keyval;

        add_DrvGtkKeybordState(a->press, a->release, a->key_transform_table, &tmp);

        write_c_DrvGtkKeyRingBuffer(a->key_ring_buffer, &tmp);

        return TRUE;
}

static gboolean release_key_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;
        GdkEventKey *key = (GdkEventKey*)event;

        struct DrvGtkKey tmp;
        tmp.state = DrvGtkKeyState_release;
        tmp.value = key->keyval;

        add_DrvGtkKeybordState(a->press, a->release, a->key_transform_table, &tmp);

        write_c_DrvGtkKeyRingBuffer(a->key_ring_buffer, &tmp);

        return TRUE;
}

static gboolean realize_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        GdkWindow *gdk_window = gtk_widget_get_window(wgt);
        gdk_window_set_cursor(gdk_window, gdk_cursor_new(GDK_BLANK_CURSOR));

        return TRUE;
}

static void init_signal_MainWindow(struct MainWindow *a)
{
        g_signal_connect(G_OBJECT(a->wgt), "realize",
                         G_CALLBACK(realize_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "key-press-event",
                         G_CALLBACK(press_key_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "key-release-event",
                         G_CALLBACK(release_key_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "destroy",
                         G_CALLBACK(gtk_main_quit), NULL);
}

static void init_screen(struct MainWindow* a, const gint width, const gint height)
{
        a->frame_buffer_width  = width;
        a->frame_buffer_height = height;

        a->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
        a->screen = gtk_image_new_from_pixbuf(a->pixbuf);

        a->frame_buffer = gdk_pixbuf_get_pixels(a->pixbuf);

        gtk_widget_set_size_request(a->screen, a->frame_buffer_width, a->frame_buffer_height);
        gtk_widget_set_double_buffered(a->screen, TRUE);
        gtk_container_add(GTK_CONTAINER(a->wgt), a->screen);
}

struct MainWindow* new_MainWindow(struct DrvGtkKeyRingBuffer *key_ring_buffer,
                                  struct DrvGtkKeybordState *press,
                                  struct DrvGtkKeybordState *release,
                                  struct DrvGtkKeybordState *key_transform_table)
{
        struct MainWindow *a = g_malloc(sizeof(*a));

        a->wgt = gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(a->wgt), " ");

        a->gdk_display = gdk_display_get_default();
        a->gdk_screen = gdk_display_get_default_screen(a->gdk_display);
        a->gdk_device_manager = gdk_display_get_device_manager(a->gdk_display);
        a->gdk_device = gdk_device_manager_get_client_pointer(a->gdk_device_manager);

        init_screen(a, 64, 64);

        a->key_ring_buffer = key_ring_buffer;

        a->press = press;
        a->release = release;
        a->key_transform_table = key_transform_table;

        init_signal_MainWindow(a);

        return a;
}

void redraw_MainWindow(struct MainWindow *a)
{
        gtk_widget_hide(a->screen);
        gtk_widget_show_now(a->screen);
}

void show_MainWindow(struct MainWindow *a)
{
        gtk_widget_show_all(a->wgt);
        redraw_MainWindow(a);
}

void hide_MainWindow(struct MainWindow *a)
{
        gtk_widget_hide(a->wgt);
}

static void resize_screen(struct MainWindow *a, const gint width, const gint height)
{
        gtk_container_remove(GTK_CONTAINER(a->wgt), a->screen);

        a->frame_buffer_width  = width;
        a->frame_buffer_height = height;

        a->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, width, height);
        a->screen = gtk_image_new_from_pixbuf(a->pixbuf);

        gtk_widget_set_size_request(a->screen, a->frame_buffer_width, a->frame_buffer_height);

        a->frame_buffer = gdk_pixbuf_get_pixels(a->pixbuf);

        gtk_container_add(GTK_CONTAINER(a->wgt), a->screen);

        gtk_widget_show(a->screen);
}

void resize_MainWindow(struct MainWindow *a, const gint width, const gint height)
{
        gtk_window_resize((GtkWindow*)(a->wgt), width, height);
        resize_screen(a, width, height);
}

void set_cursor_pos_MainWindow(struct MainWindow *a, const gint x, const gint y)
{
        gdk_device_warp(a->gdk_device, a->gdk_screen, x, y);
}
