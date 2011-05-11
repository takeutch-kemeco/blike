#include "blikedrv.h"

#define	w	bl_work

void bl_drawPtrn_r(int sx, int sy, int x0, int y0, const char *c, const char *p)
{
	BL_READY_WINDOW
	bl_setPtrn0(sx, sy, w.win[BL_SLCTWIN].xsiz, 0x80000000, w.win[BL_SLCTWIN].buf + x0 + y0 * w.win[BL_SLCTWIN].xsiz, c, p, bl_drawPtrn_err_r);
	return;
}
