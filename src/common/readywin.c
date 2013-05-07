#include "blikedrv.h"

#define w bl_work

void bl_readyWin(int n)
{
        int sx = w.win[0].xsiz, sy = w.win[0].ysiz;
        if (n > 0) {
                if (sx <= 0) {
                        bl_openVWin(0, 640, 400);
                        goto set640x400;
                }
        } else {
set640x400:
                sx = 640;
                sy = 400;
        }
        bl_openVWin(n, sx, sy);
        return;
}
