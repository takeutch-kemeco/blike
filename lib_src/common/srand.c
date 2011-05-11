#include "blikedrv.h"

#define	w	bl_work

void bl_srand(int seed)
{
	w.rand_seed = seed;
	w.mod |= BL_READYRANDSEED;
	return;
}
