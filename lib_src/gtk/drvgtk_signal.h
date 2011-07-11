#include <glib.h>

#ifndef __DRVGTK_SIGNAL_H__
#define __DRVGTK_SIGNAL_H__

struct DrvGtkSignal_resize_window {
	gboolean	ready;
	gint		width;
	gint		height;
};

struct DrvGtkSignal_show_window {
	gboolean	ready;
};

struct DrvGtkSignal_flash_window {
	gboolean	ready;
	gpointer	src_frame_buffer;
};

struct DrvGtkSignal_exit_window {
	gboolean	ready;
};

struct DrvGtkSignal {
	struct DrvGtkSignal_resize_window	resize_window;
	struct DrvGtkSignal_show_window		show_window;
	struct DrvGtkSignal_flash_window	flash_window;
	struct DrvGtkSignal_exit_window		exit_window;
};

extern struct DrvGtkSignal* new_DrvGtkSignal(void);
extern void free_DrvGtkSignal(struct DrvGtkSignal* a);

#endif // __DRVGTK_SIGNAL_H__
