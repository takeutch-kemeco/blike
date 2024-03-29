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

#pragma once

#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"

typedef void (*bld_callback_key_press_MainWindow)(void* a, const int keyval);
typedef void (*bld_callback_key_release_MainWindow)(void* a, const int keyval);

typedef void (*bld_callback_motion_notify_MainWindow)(void* a,
                                                      const double pos_x,
                                                      const double pos_y,
                                                      const double pressure,
                                                      const double angle_x,
                                                      const double angle_y);

typedef void (*bld_callback_button_press_MainWindow)(void* a, const int button_number);
typedef void (*bld_callback_button_release_MainWindow)(void* a, const int button_number);

struct MainWindow {
        GtkEventController *event_controller_key; // キーボード入力監視

        GtkEventController *event_controller_motion; // マウス座標監視
        GtkGesture *gesture_click_primary; // マウスクリック監視（選択ボタン）
        GtkGesture *gesture_click_secondary; // マウスクリック監視（キャンセルボタン）

        GtkWidget *wgt;
        GtkWidget *frame;
        GtkWidget *drawing_area;
        GdkPixbuf *pixbuf;
        guchar *frame_buffer;
        gint frame_buffer_width;
        gint frame_buffer_height;
        gint screen_offset_x;
        gint screen_offset_y;

        struct DrvGtkKeyRingBuffer *key_ring_buffer;
        struct DrvGtkKeybordState *press;
        struct DrvGtkKeybordState *release;
        struct DrvGtkKeybordState *key_transform_table;

        bld_callback_key_press_MainWindow callback_key_press;
        bld_callback_key_release_MainWindow callback_key_release;

        bld_callback_motion_notify_MainWindow callback_motion_notify;
        bld_callback_button_press_MainWindow callback_button_press;
        bld_callback_button_release_MainWindow callback_button_release;
        void* callback_arg;
};

struct MainWindow* new_MainWindow(GtkApplication *app,
                                  struct DrvGtkKeyRingBuffer *key_ring_buffer,
                                  struct DrvGtkKeybordState *press,
                                  struct DrvGtkKeybordState *release,
                                  struct DrvGtkKeybordState *key_transform_table);
