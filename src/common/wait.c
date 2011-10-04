#include "blikedrv.h"

#define	w	bl_work

void bl_wait(int msec)
{
	BL_READY_WINDOW0
	bl_flshWin(w.win[0].xsiz, w.win[0].ysiz, 0, 0);
	bl_waitNF(msec);
	return;
}
