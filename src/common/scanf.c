#include "blikedrv.h"

#define	w	bl_work

int bl_scanf(const char *s, ...)
{
	int r;
	va_list ap;
	va_start(ap, s);
	r = bl_vscanf(s, ap);
	va_end(ap);
	return r;
}
