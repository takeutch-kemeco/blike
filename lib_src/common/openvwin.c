#include "blikedrv.h"

#define	w	bl_work

void bl_openVWin(int n, int sx, int sy)
{
	int i, x, y;
	if (n < 0 || n > BL_DBGWIN) return;
	if (w.win[n].xsiz != 0) return;
	if (sx <= 0 || sy <= 0) return;
	if (n == BL_DBGWIN) {
		if (w.win[0].xsiz == 0) return; /* 先に作ってはいけない */
		if (sx != w.win[0].xsiz || sy != w.win[0].ysiz) return;
	}
	if (n == 0) {
		if (sx < 160) return;
		w.cbuf = NULL;
		w.ccol = NULL;
		w.cbak = NULL;
	}
	w.win[n].buf = bld_malloc(sx * sy * sizeof (int));
	if (w.win[n].buf == NULL) {
err:
		BL_EXIT
	}
	w.win[n].xsiz = sx;
	w.win[n].ysiz = sy;
	if (n == 0) {
		w.csiz_x = (sx + 7) / 8;
		w.csiz_y = (sy + 7) / 8;
		w.cbuf = bld_malloc(w.csiz_x * w.csiz_y * sizeof (int));
		w.ccol = bld_malloc(w.csiz_x * w.csiz_y * sizeof (int));
		w.cbak = bld_malloc(w.csiz_x * w.csiz_y * sizeof (int));
		if (w.cbuf == NULL || w.ccol == NULL || w.cbak == NULL)
			goto err;
	}
	if (n != BL_DBGWIN) {
		i = w.slctwin;
		w.slctwin = n;
		w.win[BL_SLCTWIN] = w.win[n];
		bl_cls();
		w.slctwin = i;
		w.win[BL_SLCTWIN] = w.win[i];
	} else {
		/* わざとゴミで埋める */
		i = w.slctwin;
		w.slctwin = n;
		w.win[BL_SLCTWIN] = w.win[n];
		for (y = 0; y < sy; y++) {
			for (x = 0; x < sx; x++)
				w.win[BL_SLCTWIN].buf[x + y * sx] = bl_iCol((x + y) & 7);
		}
		w.slctwin = i;
		w.win[BL_SLCTWIN] = w.win[i];
	}
	i = w.mod & (BL_DBGFLSH | BL_RLSFLSH);
	if (i == BL_DBGFLSH)
		w.win[BL_SYSWIN] = w.win[BL_DBGWIN];
	if (i == BL_RLSFLSH)
		w.win[BL_SYSWIN] = w.win[0];
	if (n == 0)
		bld_openWin(sx, sy);
	if (n == BL_DBGWIN) {
		i = w.mod & (BL_DBGFLSH | BL_RLSFLSH);
		if (i == BL_DBGFLSH)
			bld_flshWin(sx, sy, 0, 0);
	}
	return;
}
