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
#include "drvgtk_signal.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"
#include "drvgtk_translate_keycode.h"
#include "drvgtk_sleep.h"

static gboolean
key_press_window(
        GtkEventControllerKey *self,
        guint keyval,
        guint keycode,
        GdkModifierType state,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        struct DrvGtkKey tmp = {keyval, DrvGtkKeyState_press};
        add_DrvGtkKeybordState(a->press, a->release, a->key_transform_table, &tmp);
        write_c_DrvGtkKeyRingBuffer(a->key_ring_buffer, &tmp);

        if (a->callback_key_press != NULL)
                a->callback_key_press(a->callback_arg, __drvgtk_translate_keycode_gtk_to_osecpu(&tmp));

        return TRUE;
}

static gboolean
key_release_window(
        GtkEventControllerKey *self,
        guint keyval,
        guint keycode,
        GdkModifierType state,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        struct DrvGtkKey tmp = {keyval, DrvGtkKeyState_release};
        add_DrvGtkKeybordState(a->press, a->release, a->key_transform_table, &tmp);
        write_c_DrvGtkKeyRingBuffer(a->key_ring_buffer, &tmp);

        if (a->callback_key_release != NULL)
                a->callback_key_release(a->callback_arg, __drvgtk_translate_keycode_gtk_to_osecpu(&tmp));

        return TRUE;
}

static void
mouse_motion_window(
        GtkEventControllerMotion* self,
        gdouble x,
        gdouble y,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        const gdouble pos_x = x - a->screen_offset_x;
        const gdouble pos_y = y - a->screen_offset_y;
        const gdouble pressure = 0;
        const gdouble angle_x = 0;
        const gdouble angle_y = 0;

        if (a->callback_motion_notify != NULL)
                a->callback_motion_notify(a->callback_arg, pos_x, pos_y, pressure, angle_x, angle_y);

#ifdef DEBUG_MOUSE
        g_print("motion_notify_MainWindow(), pos x,y:[%f, %f], angle x, y:[%f, %f], pressure:[%f]\n",
                pos_x, pos_y, angle_x, angle_y, pressure);
#endif // DEBUG_MOUSE
}

static void
mouse_press_primary_window(
        GtkGestureClick* self,
        gint n_press,
        gdouble x,
        gdouble y,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        gint flag = 1;

        if (a->callback_button_press != NULL)
                a->callback_button_press(a->callback_arg, flag);
}

static void
mouse_release_primary_window(
        GtkGestureClick* self,
        gint n_press,
        gdouble x,
        gdouble y,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        gint flag = 1;

        if (a->callback_button_release != NULL)
                a->callback_button_release(a->callback_arg, flag);
}

static void
mouse_press_secondary_window(
        GtkGestureClick* self,
        gint n_press,
        gdouble x,
        gdouble y,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        gint flag = 2;

        if (a->callback_button_press != NULL)
                a->callback_button_press(a->callback_arg, flag);
}

static void
mouse_release_secondary_window(
        GtkGestureClick* self,
        gint n_press,
        gdouble x,
        gdouble y,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        gint flag = 2;

        if (a->callback_button_release != NULL)
                a->callback_button_release(a->callback_arg, flag);
}

static gboolean realize_window(GtkWindow *window, gpointer user_data)
{
        // とくに何もしない

        return TRUE;
}

static gboolean unrealize_window(GtkWindow *window, gpointer user_data)
{
        // とくに何もしない

        return TRUE;
}

static void redraw_window(struct MainWindow *a)
{
        // 再描画　（もっと良い方法無い？？？）
        gtk_widget_hide(a->frame);
        gtk_widget_show(a->frame);
}

static void
draw_window(GtkDrawingArea *drawing_area,
            cairo_t *cr,
            int width,
            int height,
            gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;
        gdk_cairo_set_source_pixbuf(cr, a->pixbuf, 0, 0);
        cairo_paint (cr);
}

static void
resize_window(
        GtkWindow *window,
        const gint width,
        const gint height,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        a->frame_buffer_width  = width;
        a->frame_buffer_height = height;

        a->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, TRUE, 8, width, height); // RGBA
        a->drawing_area = gtk_image_new_from_pixbuf(a->pixbuf);

        gtk_widget_set_size_request(a->drawing_area, a->frame_buffer_width, a->frame_buffer_height);

        a->frame_buffer = gdk_pixbuf_get_pixels(a->pixbuf);

        gtk_widget_show(a->drawing_area);
}

static gint round_range(gint a, gint min, gint max)
{
        if (a > max)
                a = max;
        else if (a < min)
                a = min;

        return a;
}

static void __flash_window(GtkWindow *window, gint x, gint y, gint w, gint h);
static void __resize_window(GtkWindow *window, gint w, gint h);
static void __show_window(GtkWindow *window);
static void __exit_window(GtkWindow *window);

static void __flash_window(GtkWindow *window, gint x, gint y, gint w, gint h)
{
        struct DrvGtkPthreadData *a = drvgtk_pthread_data;

        gpointer src_frame_buffer = a->bl_work->win[0].buf;

        if (src_frame_buffer == NULL)
                return;

        if (a->main_window->frame_buffer == NULL)
                return;

        round_range(x, 0, a->main_window->frame_buffer_width - 1);
        round_range(y, 0, a->main_window->frame_buffer_height - 1);

        round_range(w, 0, a->main_window->frame_buffer_width - x);
        round_range(h, 0, a->main_window->frame_buffer_height - y);

        const guint32 top_ofst = (y * a->main_window->frame_buffer_width) + x;
        const guint32 src_top_ofst = top_ofst * 4;
        const guint32 dst_top_ofst = top_ofst * 4;

        guchar *src = ((guchar*)src_frame_buffer) + src_top_ofst;
        guchar *dst = ((guchar*)a->main_window->frame_buffer) + dst_top_ofst;

        const guint32 next_ofst =  a->main_window->frame_buffer_width - w;
        const guint32 src_next_ofst = next_ofst * 4;
        const guint32 dst_next_ofst = next_ofst * 4;

        for (int j = 0; j < h; j++) {
                for (int i = 0; i < w; i++) {
                        // gdk-pixbuf: RGBA <- Windows: BGRX
                        // ただし A <- 0xff 固定
#if G_BYTE_ORDER == G_BIG_ENDIAN
                        dst[0] = 0xff;   // gdk A <- 0 --- 0 <- win X
                        dst[1] = src[3]; // gdk B <- 3 \ / 1 <- win R
                        dst[2] = src[2]; // gdk G <- 2 -.- 2 <- win G
                        dst[3] = src[1]; // gdk R <- 1 / \ 3 <- win B
                        dst += 4;
                        src += 4;

#elif G_BYTE_ORDER == G_LITTLE_ENDIAN
                        dst[0] = src[2]; // gdk R <- 2 \ / 0 <- win B
                        dst[1] = src[1]; // gdk G <- 1 -.- 1 <- win G
                        dst[2] = src[0]; // gdk B <- 0 / \ 2 <- win R
                        dst[3] = 0xff;   // gdk A <- 3 --- 3 <- win X

                        dst += 4;
                        src += 4;

#else
#error "System-endian is unknown."
#endif // G_BYTE_ORDER

                }
                dst += dst_next_ofst;
                src += src_next_ofst;
        }

        gtk_widget_queue_draw(a->main_window->drawing_area);
        redraw_window(a->main_window);
}

static void __resize_window(GtkWindow *window, gint w, gint h)
{
        struct DrvGtkPthreadData *a = drvgtk_pthread_data;

        gtk_window_set_default_size(window, w, h);
        resize_window(window, w, h, (gpointer)a->main_window);

        __show_window(window);
}

static void __show_window(GtkWindow *window)
{
        struct DrvGtkPthreadData *a = drvgtk_pthread_data;

        gtk_widget_queue_draw(a->main_window->drawing_area);
        redraw_window(a->main_window);

        gtk_widget_show(GTK_WIDGET(window));
}

static void __exit_window(GtkWindow *window)
{
        struct DrvGtkPthreadData *a = drvgtk_pthread_data;

        g_application_quit(G_APPLICATION(a->app));
}

static void init_signal_window(GtkWindow *window, gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        g_timeout_add_full(G_PRIORITY_HIGH, DRVGTK_SYGNAL_CHECK_INTERVAL_MS, update_DrvGtkSignalChain, (gpointer)drvgtk_pthread_data, NULL);

        g_signal_connect(GTK_WINDOW(window), "realize", G_CALLBACK(realize_window), a);
        g_signal_connect(GTK_WINDOW(window), "unrealize", G_CALLBACK(unrealize_window), a);

        g_signal_connect(GTK_WINDOW(window), "drvgtk-flash-window", G_CALLBACK(__flash_window), NULL);
        g_signal_connect(GTK_WINDOW(window), "drvgtk-resize-window", G_CALLBACK(__resize_window), NULL);
        g_signal_connect(GTK_WINDOW(window), "drvgtk-show-window", G_CALLBACK(__show_window), NULL);
        g_signal_connect(GTK_WINDOW(window), "drvgtk-exit-window", G_CALLBACK(__exit_window), NULL);

        g_signal_connect(a->event_controller_key, "key-pressed", G_CALLBACK(key_press_window), user_data);
        g_signal_connect(a->event_controller_key, "key-released", G_CALLBACK(key_release_window), user_data);

        g_signal_connect(a->event_controller_motion, "motion", G_CALLBACK(mouse_motion_window), user_data);

        g_signal_connect(GTK_GESTURE(a->gesture_click_primary), "pressed", G_CALLBACK(mouse_press_primary_window), user_data);
        g_signal_connect(GTK_GESTURE(a->gesture_click_primary), "released", G_CALLBACK(mouse_release_primary_window), user_data);

        g_signal_connect(GTK_GESTURE(a->gesture_click_secondary), "pressed", G_CALLBACK(mouse_press_secondary_window), user_data);
        g_signal_connect(GTK_GESTURE(a->gesture_click_secondary), "released", G_CALLBACK(mouse_release_secondary_window),user_data);
}

// window に独自シグナルを追加
static void add_signal_window(GtkWindow *window)
{
        // インスタンス(window)のクラス(GtkWindowClass)のクラスインスタンスを得る
        GObjectClass *window_class = G_OBJECT_GET_CLASS(G_OBJECT(window));

        // ウィンドウの画像更新シグナル
        g_signal_new("drvgtk-flash-window", G_TYPE_FROM_CLASS(window_class),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS,
                     0, NULL, NULL, NULL, G_TYPE_NONE, 4, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT, G_TYPE_INT);

        // ウインドウのリサイズシグナル
        g_signal_new("drvgtk-resize-window", G_TYPE_FROM_CLASS(window_class),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS,
                     0, NULL, NULL, NULL, G_TYPE_NONE, 2, G_TYPE_INT, G_TYPE_INT);

        // ウインドウの表示シグナル
        g_signal_new("drvgtk-show-window", G_TYPE_FROM_CLASS(window_class),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS,
                     0, NULL, NULL, NULL, G_TYPE_NONE, 0);

        // ウインドウの終了シグナル
        g_signal_new("drvgtk-exit-window", G_TYPE_FROM_CLASS(window_class),
                     G_SIGNAL_RUN_LAST | G_SIGNAL_NO_RECURSE | G_SIGNAL_NO_HOOKS,
                     0, NULL, NULL, NULL, G_TYPE_NONE, 0);
}

static void activate_window(GtkWindow *window, gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        add_signal_window(window); // window に独自シグナルを追加

        gtk_window_set_title(GTK_WINDOW(window), " ");

        a->frame = gtk_frame_new(NULL);
        gtk_window_set_child(GTK_WINDOW(window), a->frame);

        a->drawing_area = gtk_drawing_area_new();
        gtk_widget_set_size_request(a->drawing_area, a->frame_buffer_width, a->frame_buffer_height);
        gtk_frame_set_child(GTK_FRAME(a->frame), a->drawing_area);
        gtk_drawing_area_set_draw_func(GTK_DRAWING_AREA(a->drawing_area), draw_window, user_data, NULL);

        a->pixbuf = gdk_pixbuf_new(GDK_COLORSPACE_RGB, FALSE, 8, a->frame_buffer_width, a->frame_buffer_height);
        a->drawing_area = gtk_image_new_from_pixbuf(a->pixbuf);

        a->frame_buffer = gdk_pixbuf_get_pixels(a->pixbuf);

        // キーボード入力コントローラーを、ウインドウへ登録
        a->event_controller_key = gtk_event_controller_key_new();
        gtk_widget_add_controller(GTK_WIDGET(window), a->event_controller_key);

        // マウス入力（位置）コントローラーを、ウインドウへ登録
        a->event_controller_motion = gtk_event_controller_motion_new();
        gtk_widget_add_controller(GTK_WIDGET(window), a->event_controller_motion);

        // マウス入力（選択ボタン）コントローラーを、ウインドウへ登録
        a->gesture_click_primary = gtk_gesture_click_new();
        gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(a->gesture_click_primary), GDK_BUTTON_PRIMARY);
        gtk_widget_add_controller(GTK_WIDGET(window), GTK_EVENT_CONTROLLER(a->gesture_click_primary));

        // マウス入力（キャンセルボタン）コントローラーを、ウインドウへ登録
        a->gesture_click_secondary = gtk_gesture_click_new();
        gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(a->gesture_click_secondary), GDK_BUTTON_SECONDARY);
        gtk_widget_add_controller(GTK_WIDGET(window), GTK_EVENT_CONTROLLER(a->gesture_click_secondary));

        init_signal_window(window, user_data);
}

static gpointer thread_bl_main(gpointer data)
{
        struct DrvGtkPthreadData *a = (struct DrvGtkPthreadData*)data;

        // MainWindowが登録されるまで待機
        // スピンロック
        while (g_application_get_is_registered(G_APPLICATION(a->app)) == FALSE) {
                drvgtk_msleep(DRVGTK_SYGNAL_CHECK_INTERVAL_MS);
        }

        a->control_program(); // bl_main()

        return NULL;
}

static void
activate_GtkApplication(
        GtkApplication *app,
        gpointer user_data)
{
        struct MainWindow *a = (struct MainWindow*)user_data;

        drvgtk_pthread_data->ptid = g_thread_new("thread_bl_main", thread_bl_main, (gpointer)drvgtk_pthread_data);

        a->wgt = gtk_application_window_new(app);
        activate_window(GTK_WINDOW(a->wgt), user_data);
}

struct MainWindow* new_MainWindow(GtkApplication *app,
                                  struct DrvGtkKeyRingBuffer *key_ring_buffer,
                                  struct DrvGtkKeybordState *press,
                                  struct DrvGtkKeybordState *release,
                                  struct DrvGtkKeybordState *key_transform_table)
{
        struct MainWindow *a = g_malloc(sizeof(*a));

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

        a->frame_buffer = NULL;
        a->frame_buffer_width  = 64;
        a->frame_buffer_height = 64;
        a->screen_offset_x = 0;
        a->screen_offset_y = 0;

        g_signal_connect(app, "activate", G_CALLBACK(activate_GtkApplication), a);

        return a;
}
