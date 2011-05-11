#include "blikedrv.h"

#define	w	bl_work

void bl_fillOval(int sx, int sy, int x0, int y0)
{
	int x, y;
	double cx = sx * 0.5 + 0.5, cy = sy * 0.5 + 0.5, dx2, dy2, r2 = cx * cy;
	r2 *= r2;
	for (y = 0; y < sy; y++) {
		dy2 = (y - cy + 1) * cx;
		dy2 *= dy2;
		for (x = 0; x < sx; x++) {
			dx2 = (x - cx + 1) * cy;
			dx2 *= dx2;
			if (dx2 + dy2 <= r2)
				bl_setPix(x0 + x, y0 + y, w.col0);
		}
	}
	return;
}
