#include <glib.h>

void drvgtk_msleep(guint t)
{
	g_usleep(t * 1000);
}
