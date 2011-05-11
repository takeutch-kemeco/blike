#include "blikedrv.h"

#define	w	bl_work

int bl_rgb(int r, int g, int b)
{
	return r << 16 | g << 8 | b;
}
