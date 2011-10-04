#include "blikedrv.h"

#define	w	bl_work

void bl_slctWin(int n)
{
	if (0 <= n && n < BL_DBGWIN) {
		w.slctwin = n;
		w.win[BL_SLCTWIN] = w.win[n];
	}
	return;
}
