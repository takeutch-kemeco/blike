#include "blikedrv.h"

#define	w	bl_work

int bl_inkey1()
{
	return bl_inkey(BL_GETKEY | BL_CLEARREP | BL_DELFFF);
}

extern void bld_inkey(int flags);

int bl_inkey(int flags)
{
	BL_READY_WINDOW
	bld_inkey(flags);
}


