/* c_blike_01f_linux -
 * Copyright (C) 2011, 2012, 2013 Kemeco Takeutch <takeutchkemeco@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name of the Kemeco Takeutch nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <gdk/gdkkeysyms.h>
#include "config.h"

#include "main_window.h"
#include "drvgtk_pthread.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"
#include "drvgtk_translate_keycode.h"

static gboolean key_press_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;
        GdkEventKey *key = (GdkEventKey*)event;

        struct DrvGtkKey tmp;
        tmp.state = DrvGtkKeyState_press;
        tmp.value = key->keyval;

        add_DrvGtkKeybordState(a->press, a->release, a->key_transform_table, &tmp);

        write_c_DrvGtkKeyRingBuffer(a->key_ring_buffer, &tmp);

        if (a->callback_key_press != NULL)
                a->callback_key_press(a->callback_arg, __drvgtk_translate_keycode_gtk_to_osecpu(key));

#ifdef DEBUG_KEYBOARD
           g_print("key_press_MainWindow(), callback:[%p], callback_arg:{%p], keyval:[%d]\n",
                   (void*)(a->callback_key_press), (void*)(a->callback_arg),
                   __drvgtk_translate_keycode_gtk_to_vk(key->keyval));
#endif /* DEBUG_KEYBOARD */

        return TRUE;
}

static gboolean key_release_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;
        GdkEventKey *key = (GdkEventKey*)event;

        struct DrvGtkKey tmp;
        tmp.state = DrvGtkKeyState_release;
        tmp.value = key->keyval;

        add_DrvGtkKeybordState(a->press, a->release, a->key_transform_table, &tmp);

        write_c_DrvGtkKeyRingBuffer(a->key_ring_buffer, &tmp);

        if (a->callback_key_release != NULL)
                a->callback_key_release(a->callback_arg, __drvgtk_translate_keycode_gtk_to_osecpu(key));

#ifdef DEBUG_KEYBOARD
           g_print("key_release_MainWindow(), callback:[%p], callback_arg:{%p], keyval:[%d]\n",
                   (void*)(a->callback_key_release), (void*)(a->callback_arg),
                   __drvgtk_translate_keycode_gtk_to_vk(key->keyval));
#endif /* DEBUG_KEYBOARD */

        return TRUE;
}

static gboolean realize_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        GdkWindow *gdk_window = gtk_widget_get_window(wgt);
        gdk_window_set_cursor(gdk_window, gdk_cursor_new(GDK_BLANK_CURSOR));

        return TRUE;
}

static gboolean motion_notify_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;
        GdkEventButton *button = (GdkEventButton*)event;

        const gdouble pos_x = button->x - a->screen_offset_x;
        const gdouble pos_y = button->y - a->screen_offset_y;
        const gdouble pressure = button->axes[2];
        const gdouble angle_x = button->axes[3];
        const gdouble angle_y = button->axes[4];

        if (a->callback_motion_notify != NULL)
                a->callback_motion_notify(a->callback_arg, pos_x, pos_y, pressure, angle_x, angle_y);

#ifdef DEBUG_MOUSE
        g_print("motion_notify_MainWindow(), pos x,y:[%f, %f], angle x, y:[%f, %f], pressure:[%f]\n",
                pos_x, pos_y, angle_x, angle_y, pressure);
#endif /* DEBUG_MOUSE */

        return TRUE;
}

static gboolean button_press_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;
        GdkEventButton *button = (GdkEventButton*)event;

        gint flag = 0;
        switch (button->button) {
        case 1: flag |= (1 << 0); break;
        case 2: flag |= (1 << 1); break;
        case 3: flag |= (1 << 2); break;
        default: flag = 0;
        }

        if (a->callback_button_press != NULL)
                a->callback_button_press(a->callback_arg, flag);

#ifdef DEBUG_MOUSE
        g_print("button_press_MainWindow(), button:[%x], flag:[%x]\n", button->button, flag);
#endif /* DEBUG_MOUSE */

        return TRUE;
}

static gboolean button_release_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;
        GdkEventButton *button = (GdkEventButton*)event;

        gint flag = 0;
        switch (button->button) {
        case 1: flag |= (1 << 0); break;
        case 2: flag |= (1 << 1); break;
        case 3: flag |= (1 << 2); break;
        default: flag = 0;
        }

        if (a->callback_button_release != NULL)
                a->callback_button_release(a->callback_arg, flag);

#ifdef DEBUG_MOUSE
        g_print("button_release_MainWindow(), button:[%x], flag:[%x]\n", button->button, flag);
#endif /* DEBUG_MOUSE */

        return TRUE;
}

static gboolean configure_MainWindow(GtkWidget *wgt, GdkEventExpose *event, gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;

        GdkWindow *gdk_window = gtk_widget_get_window(wgt);
        gint wx, wy, ww, wh;
        gdk_window_get_geometry(gdk_window, &wx, &wy, &ww, &wh);
#ifdef DEBUG_MOUSE
        g_print("configure_MainWindow(), window global pos x,y,w,h:[%d, %d, %d, %d]\n",
                wx, wy, ww, wh);
#endif /* DEBUG_MOUSE */

        a->screen_offset_x = (ww / 2) - (a->frame_buffer_width / 2);
        a->screen_offset_y = (wh / 2) - (a->frame_buffer_height / 2);
#ifdef DEBUG_MOUSE
        g_print("configure_MainWindow(), screen offset x,y:[%d, %d]\n",
                a->screen_offset_x, a->screen_offset_y);
#endif /* DEBUG_MOUSE */

        return FALSE; /* 重要 */
}

static gboolean timeout_redraw_MainWindow(gpointer data)
{
        struct MainWindow *a = (struct MainWindow*)data;
        if (a->redraw_request) {
                gtk_image_set_from_pixbuf((GTK_IMAGE(a->screen)), a->pixbuf);
                a->redraw_request = FALSE;
        }

        return TRUE;
}

static void init_signal_MainWindow(struct MainWindow *a)
{
        g_signal_connect(G_OBJECT(a->wgt), "realize",
                         G_CALLBACK(realize_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "configure-event",
                         G_CALLBACK(configure_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "destroy",
                         G_CALLBACK(gtk_main_quit), NULL);

        g_timeout_add(DRVGTK_SYGNAL_CHECK_INTERVAL,
                      timeout_redraw_MainWindow, (gpointer)a);

        g_signal_connect(G_OBJECT(a->wgt), "key-press-event",
                         G_CALLBACK(key_press_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "key-release-event",
                         G_CALLBACK(key_release_MainWindow), a);

        g_signal_connect(G_OBJECT(a->wgt), "motion-notify-event",
                         G_CALLBACK(motion_notify_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "button-press-event",
                         G_CALLBACK(button_press_MainWindow), a);
        g_signal_connect(G_OBJECT(a->wgt), "button-release-event",
                         G_CALLBACK(button_release_MainWindow), a);

        gtk_widget_set_events(a->wgt, gtk_widget_get_events(a->wgt) | GDK_ALL_EVENTS_MASK);
}

static void init_screen(struct MainWindow* a)
{
        a->frame_buffer_width  = 64;
        a->frame_buffer_height = 64;
        a->screen_offset_x = 0;
        a->screen_offset_y = 0;

        a->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8,
                                   a->frame_buffer_width, a->frame_buffer_height);

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

        init_screen(a);

        a->redraw_request = FALSE;

        a->key_ring_buffer = key_ring_buffer;

        a->press = press;
        a->release = release;
        a->key_transform_table = key_transform_table;

        a->callback_key_press = NULL;
        a->callback_key_release = NULL;

        a->callback_motion_notify = NULL;
        a->callback_button_press = NULL;
        a->callback_button_release = NULL;

        a->callback_arg = NULL;

        init_signal_MainWindow(a);

        return a;
}

void redraw_MainWindow(struct MainWindow *a,
                       const gint x, const gint y, const gint w, const gint h)
{
        gtk_widget_queue_draw_area(a->screen,
                                   a->screen_offset_x + x,
                                   a->screen_offset_y + y,
                                   w,
                                   h);

        a->redraw_request = TRUE;
}

void show_MainWindow(struct MainWindow *a)
{
        redraw_MainWindow(a,
                          a->screen_offset_x,
                          a->screen_offset_y,
                          a->frame_buffer_width,
                          a->frame_buffer_height);

        gtk_widget_show_all(a->wgt);
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
        GdkWindow *gdk_window = gtk_widget_get_window(a->wgt);

        gint offx;
        gint offy;
        gdk_window_get_position(gdk_window, &offx, &offy);

        offx += a->screen_offset_x;
        offy += a->screen_offset_y;

        gdk_device_warp(a->gdk_device, a->gdk_screen, offx + x, offy + y);
}
