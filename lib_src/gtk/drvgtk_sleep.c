#include <glib.h>
#include "drvgtk_build_flag.h"

void drvgtk_msleep(guint t)
{
	g_usleep(t * 1000);
}
