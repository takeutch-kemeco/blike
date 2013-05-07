#include "blikedrv.h"

#define w bl_work

int bl_rand()
{
        if ((w.mod & BL_READYRANDSEED) == 0) {
                bl_srand(bld_getSeed());
                w.mod |= BL_READYRANDSEED;
        }
        w.rand_seed = w.rand_seed * 1103515245 + 12345;
        return (int) (w.rand_seed >> 16) & 0x7fff;
}
