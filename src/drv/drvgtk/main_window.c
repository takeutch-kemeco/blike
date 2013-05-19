#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "config.h"

#include "main_window.h"
#include "main_screen.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keybord_state.h"

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

static void init_signal_MainWindow(struct MainWindow *a)
{
        g_signal_connect(G_OBJECT(a->wgt), "key-press-event",
                         G_CALLBACK(press_key_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "key-release-event",
                         G_CALLBACK(release_key_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "destroy",
                         G_CALLBACK(gtk_main_quit), NULL);
}

struct MainWindow* new_MainWindow(struct DrvGtkKeyRingBuffer *key_ring_buffer,
                                  struct DrvGtkKeybordState *press,
                                  struct DrvGtkKeybordState *release,
                                  struct DrvGtkKeybordState *key_transform_table)
{
        struct MainWindow *a = g_malloc(sizeof(*a));

        a->wgt = gtk_window_new(GTK_WINDOW_TOPLEVEL);
        gtk_window_set_title(GTK_WINDOW(a->wgt), " ");

        a->key_ring_buffer = key_ring_buffer;

        a->press = press;
        a->release = release;
        a->key_transform_table = key_transform_table;

        init_signal_MainWindow(a);

        return a;
}

void show_MainWindow(struct MainWindow *a)
{
        gtk_widget_show_all(a->wgt);
}



void hide_MainWindow(struct MainWindow *a)
{
        gtk_widget_hide(a->wgt);
}

void resize_MainWindow(struct MainWindow *a, gint width, gint height)
{
        gtk_window_resize((GtkWindow*)(a->wgt), width, height);
}
