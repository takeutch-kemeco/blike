#include "blikedrv.h"

#define	w	bl_work

int bl_rnd(int max_puls_1)
{
	int r;
	r = bl_rand();	/* 以下、RAND_MAX == 0x7fff を仮定 */
	if (max_puls_1 > 4096)
		r |= bl_rand() << 15;
	return r % max_puls_1;
}
