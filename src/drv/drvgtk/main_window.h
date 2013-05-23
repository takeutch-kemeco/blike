#include <gtk/gtk.h>
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"

#ifndef __MAIN_WINDOW_H__
#define __MAIN_WINDOW_H__

struct MainWindow {
        GtkWidget *wgt;
        struct DrvGtkKeyRingBuffer *key_ring_buffer;
        struct DrvGtkKeybordState *press;
        struct DrvGtkKeybordState *release;
        struct DrvGtkKeybordState *key_transform_table;
};

struct MainWindow* new_MainWindow(struct DrvGtkKeyRingBuffer *key_ring_buffer,
                                  struct DrvGtkKeybordState *press,
                                  struct DrvGtkKeybordState *release,
                                  struct DrvGtkKeybordState *key_transform_table);
void show_MainWindow(struct MainWindow *a);
void hide_MainWindow(struct MainWindow *a);
void resize_MainWindow(struct MainWindow *a, gint width, gint height);

#endif //__MAIN_WINDOW_H__
