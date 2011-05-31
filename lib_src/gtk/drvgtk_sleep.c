#include <time.h>
#include <unistd.h>
#include <glib.h>

void drvgtk_msleep(guint t)
{
	struct timespec req, rem;
	req.tv_sec  = 0;
	req.tv_nsec = (1000 * 1000) * t;
	
	nanosleep(&req, &rem);
}
