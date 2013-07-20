#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"

#ifndef __MAIN_WINDOW_H__
#define __MAIN_WINDOW_H__

typedef void (*bld_callback_motion_notify_MainWindow)(void* a,
                                                      const double pos_x,
                                                      const double pos_y,
                                                      const double pressure,
                                                      const double angle_x,
                                                      const double angle_y);

typedef void (*bld_callback_button_press_MainWindow)(void* a, const int button_number);
typedef void (*bld_callback_button_release_MainWindow)(void* a, const int button_number);

struct MainWindow {
        GtkWidget *wgt;

        GdkDisplay *gdk_display;
        GdkScreen *gdk_screen;
        GdkDeviceManager *gdk_device_manager;
        GdkDevice *gdk_device;

        GtkWidget *screen;
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

        bld_callback_motion_notify_MainWindow callback_motion_notify;
        bld_callback_button_press_MainWindow callback_button_press;
        bld_callback_button_release_MainWindow callback_button_release;
        void* callback_arg;
};

struct MainWindow* new_MainWindow(struct DrvGtkKeyRingBuffer *key_ring_buffer,
                                  struct DrvGtkKeybordState *press,
                                  struct DrvGtkKeybordState *release,
                                  struct DrvGtkKeybordState *key_transform_table);

void show_MainWindow(struct MainWindow *a);
void hide_MainWindow(struct MainWindow *a);
void redraw_MainWindow(struct MainWindow *a);
void resize_MainWindow(struct MainWindow *a, const gint width, const gint height);

void set_cursor_pos_MainWindow(struct MainWindow *a, const gint x, const gint y);

#endif //__MAIN_WINDOW_H__
