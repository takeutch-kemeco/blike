#include "blikedrv.h"

#define	w	bl_work

void bl_drawPtrn_err_d(const char *msg, unsigned char *nam)
{
	bl_puts("drawPtrn : ");
	bl_puts(msg);
	bl_puts(" (");
	bl_puts(nam);
	bl_puts(")");
	bl_wait(-1);
}

void bl_drawPtrn_d(int sx, int sy, int x0, int y0, const char *c, const char *p)
{
	BL_READY_WINDOW
	bl_setPtrn0(sx, sy, w.win[BL_SLCTWIN].xsiz, 0x80000000, w.win[BL_SLCTWIN].buf + x0 + y0 * w.win[BL_SLCTWIN].xsiz, c, p, bl_drawPtrn_err_d);
	return;
}
