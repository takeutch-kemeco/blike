#include "blikedrv.h"

#define	w	bl_work

void bl_cls()
{
	int x, y;
	BL_READY_WINDOW
	if (w.slctwin == 0) {
		for (y = 0; y < w.csiz_y; y++) {
			for (x = 0; x < w.csiz_x; x++) {
				w.cbuf[x + y * w.csiz_x] = ' ';
				w.ccol[x + y * w.csiz_x] = w.col1;
				w.cbak[x + y * w.csiz_x] = w.col1;
			}
		}
		w.cx = w.cy = 0;
	}
	for (y = 0; y < w.win[BL_SLCTWIN].ysiz; y++) {
		for (x = 0; x < w.win[BL_SLCTWIN].xsiz; x++)
			w.win[BL_SLCTWIN].buf[x + y * w.win[BL_SLCTWIN].xsiz] = w.col1;
	}
	return;
}
