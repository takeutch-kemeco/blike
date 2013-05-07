#include "blikedrv.h"

#define w bl_work

void bl_locate(int x, int y)
{
        BL_READY_WINDOW
        if (0 <= x && x < w.csiz_x && 0 <= y && y <= w.csiz_y) {
                w.cx = x;
                w.cy = y * 2;
        }
        return;
}
