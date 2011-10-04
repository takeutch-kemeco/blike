#include "blikedrv.h"

#define	w	bl_work

void bl_fillRect(int sx, int sy, int x0, int y0)
{
	int x, y;
	for (y = 0; y < sy; y++) {
		for (x = 0; x < sx; x++)
			bl_setPix(x0 + x, y0 + y, w.col0);
	}
	return;
}
