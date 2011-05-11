#include "blikedrv.h"

#define	w	bl_work

void bl_waitNF(int msec)
{
	int i;
	i = w.mod & (BL_DBGFLSH | BL_RLSFLSH);
	if (i == BL_DBGFLSH && w.win[0].xsiz > 0 && w.win[BL_DBGWIN].xsiz <= 0)
		bl_readyWin(BL_DBGWIN);
	bld_flshSys();
	if (msec == -3) return;
	if (msec == -2) {
		w.tmcount0 = w.tmcount;
		return;
	}
	if (msec == -1) {
		for (;;) {
			bld_waitNF();
			bld_flshSys();
		}
	}
	while (w.tmcount - w.tmcount0 < msec) {
		bld_waitNF();
		bld_flshSys();
	}
	w.tmcount0 += msec;
	return;
}
