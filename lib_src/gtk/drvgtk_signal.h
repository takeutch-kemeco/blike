#include <glib.h>

#ifndef __DRVGTK_SIGNAL_H__
#define __DRVGTK_SIGNAL_H__

struct DrvGtkSignal_resize_window {
	gboolean	ready;
	gint		width;
	gint		height;
} __attribute__((aligned(16)));

struct DrvGtkSignal_show_window {
	gboolean	ready;
} __attribute__((aligned(16)));

struct DrvGtkSignal_flash_window {
	gboolean	ready;
	gpointer	src_frame_buffer;
} __attribute__((aligned(16)));

struct DrvGtkSignal_exit_window {
	gboolean	ready;
} __attribute__((aligned(16)));

struct DrvGtkSignal {
	struct DrvGtkSignal_resize_window	resize_window;
	struct DrvGtkSignal_show_window		show_window;
	struct DrvGtkSignal_flash_window	flash_window;
	struct DrvGtkSignal_exit_window		exit_window;
} __attribute__((aligned(16)));

extern struct DrvGtkSignal* new_DrvGtkSignal(void);
extern void free_DrvGtkSignal(struct DrvGtkSignal* a);

#endif // __DRVGTK_SIGNAL_H__
