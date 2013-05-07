#include "blikedrv.h"

#define w bl_work

void bl_setPix(int x, int y, int c)
{
        BL_READY_WINDOW
        if (0 <= x && x < w.win[BL_SLCTWIN].xsiz && 0 <= y && y < w.win[BL_SLCTWIN].ysiz) {
                int mod = w.mod & (BL_PSET | BL_PAND | BL_POR | BL_PXOR);
                if (mod == BL_PSET)
                        w.win[BL_SLCTWIN].buf[x + y * w.win[BL_SLCTWIN].xsiz] = c;
                if (mod == BL_PAND)
                        w.win[BL_SLCTWIN].buf[x + y * w.win[BL_SLCTWIN].xsiz] &= c;
                if (mod == BL_POR)
                        w.win[BL_SLCTWIN].buf[x + y * w.win[BL_SLCTWIN].xsiz] |= c;
                if (mod == BL_PXOR)
                        w.win[BL_SLCTWIN].buf[x + y * w.win[BL_SLCTWIN].xsiz] ^= c;
        }
        return;
}
