#include "blikedrv.h"

#define	w	bl_work

void bl_copyRct0(int sx, int sy, int n0, int x0, int y0, int n1, int x1, int y1)
{
	int x, y, sx0, sx1, *b0, *b1;
	if (w.win[n0].xsiz == 0) bl_readyWin(n0);
	if (w.win[n1].xsiz == 0) bl_readyWin(n1);
	if (x0 < 0) {
		sx += x0;
		x1 -= x0;
		x0 = 0;
	}
	if (y0 < 0) {
		sy += y0;
		y1 -= y0;
		y0 = 0;
	}
	if (sx > w.win[n0].xsiz - x0)
		sx = w.win[n0].xsiz - x0;
	if (sy > w.win[n0].ysiz - y0)
		sy = w.win[n0].ysiz - y0;
	if (x1 < 0) {
		sx += x1;
		x0 -= x1;
		x1 = 0;
	}
	if (y1 < 0) {
		sy += y1;
		y0 -= y1;
		y1 = 0;
	}
	if (sx > w.win[n1].xsiz - x1)
		sx = w.win[n1].xsiz - x1;
	if (sy > w.win[n1].ysiz - y1)
		sy = w.win[n1].ysiz - y1;
	b0 = w.win[n0].buf; sx0 = w.win[n0].xsiz;
	b1 = w.win[n1].buf; sx1 = w.win[n1].xsiz;
	for (y = 0; y < sy; y++) {
		for (x = 0; x < sx; x++)
			b1[(x1 + x) + (y1 + y) * sx1] = b0[(x0 + x) + (y0 + y) * sx0];
	}
	return;
}
