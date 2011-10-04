#include "blikedrv.h"

#define	w	bl_work

void bl_openWin(int x, int y)
{
	bl_openVWin(w.slctwin, x, y);
	return;
}
