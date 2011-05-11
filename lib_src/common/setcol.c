#include "blikedrv.h"

#define	w	bl_work

void bl_setCol(int c)
{
//	if (0 <= c && c <= 0xffffff)
		w.col0 = c;
	return;
}
