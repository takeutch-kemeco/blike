#include "blikedrv.h"

#define	w	bl_work

void bl_setMode(int mod)
{
	int i;
	if ((mod & (BL_PSET & BL_PAND & BL_POR & BL_PXOR)) != 0) {
		w.mod &= ~(BL_PSET | BL_PAND | BL_POR | BL_PXOR);
		w.mod |= mod & (BL_PSET | BL_PAND | BL_POR | BL_PXOR);
	}
	if ((mod & (BL_FULLHEIGHT & BL_HALFHEIGHT)) != 0) {
		w.mod &= ~(BL_FULLHEIGHT | BL_HALFHEIGHT);
		w.mod |= mod & (BL_FULLHEIGHT | BL_HALFHEIGHT);
	}
	if ((mod & (BL_DBGFLSH & BL_RLSFLSH)) != 0) {
		w.mod &= ~(BL_DBGFLSH | BL_RLSFLSH);
		w.mod |= mod & (BL_DBGFLSH | BL_RLSFLSH);
		i = mod & (BL_DBGFLSH | BL_RLSFLSH);
		if (i == BL_DBGFLSH)
			w.win[BL_SYSWIN] = w.win[BL_DBGWIN];
		if (i == BL_RLSFLSH)
			w.win[BL_SYSWIN] = w.win[0];
	}
	return;
}
