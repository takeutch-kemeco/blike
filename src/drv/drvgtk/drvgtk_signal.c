#include <glib.h>
#include "config.h"

#include "drvgtk_signal.h"

struct DrvGtkSignal* new_DrvGtkSignal(void)
{
	struct DrvGtkSignal* a = g_malloc(sizeof(*a));
	
	a->resize_window.ready = FALSE;
	a->show_window.ready   = FALSE;
	a->flash_window.ready  = FALSE;
	a->exit_window.ready   = FALSE;

	return a;
}

void free_DrvGtkSignal(struct DrvGtkSignal* a)
{
	g_free(a);
}


