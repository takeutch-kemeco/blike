#include "blike.h"

blMain()
{
    for (;;) {
		if (bl_inkey(0) == 0) wait(100);
		int i = bl_inkey(BL_WAITKEY | BL_GETKEY);
	//	int i = bl_inkey(BL_WAITKEY | BL_GETKEY | BL_DELFFF);
	//	wait(10);
		printf("%d ", i);
	}
	wait(-1);
}
