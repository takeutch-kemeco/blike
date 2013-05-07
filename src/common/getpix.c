#include "blikedrv.h"

#define w bl_work

int bl_getPix(int x, int y)
{
        BL_READY_WINDOW
        if (0 <= x && x < w.win[BL_SLCTWIN].xsiz && 0 <= y && y < w.win[BL_SLCTWIN].ysiz)
                return w.win[BL_SLCTWIN].buf[x + y * w.win[BL_SLCTWIN].xsiz];
        return -1;
}
