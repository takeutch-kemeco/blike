#include "blikedrv.h"

#define	w	bl_work

void bl_flshWin(int sx, int sy, int x0, int y0)
{
	int x, y;
	BL_READY_WINDOW0
	if (x0 < 0) {
		sx += x0;
		x0 = 0;
	}
	if (y0 < 0) {
		sy += y0;
		y0 = 0;
	}
	if (sx > w.win[0].xsiz - x0)
		sx = w.win[0].xsiz - x0;
	if (sy > w.win[0].ysiz - y0)
		sy = w.win[0].ysiz - y0;
	if (sx > 0 && sy > 0) {
		if ((w.mod & (BL_DBGFLSH | BL_RLSFLSH)) == BL_DBGFLSH) {
			if (w.win[BL_DBGWIN].xsiz == 0)
				bl_openVWin(BL_DBGWIN, w.win[0].xsiz, w.win[0].ysiz);
			bl_copyRct0(sx, sy, 0, x0, y0, BL_DBGWIN, x0, y0);
			for (y = 0; y < sy; y++) {
				for (x = 0; x < sx; x++) {
					if (((unsigned int) w.win[0].buf[(x0 + x) + (y0 + y) * w.win[0].xsiz]) > 0xffffff)
						goto err;
				}
			}
			/* 不正な色をwin[0]で使用してはいけない */
			/* 本来は描画段階ではねるべきだが(そのほうが見つけやすい)、それだとチェックが重くなるので、BL_DBGWINへの反映を阻止するという方法にした */
			/* チェックが重くなるというのは、チェック処理が遅いという意味ではなくて、チェックのための記述が多くなるという意味 */
			/* デバッグモードなので、処理が遅いのはたいした問題ではない */
		}
		bld_flshWin(sx, sy, x0, y0);
	}
err:
	return;
}
