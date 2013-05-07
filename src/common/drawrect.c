#include "blikedrv.h"

#define w bl_work

void bl_drawRect(int sx, int sy, int x0, int y0)
{
        if (sx > 0) {
                if (sy > 0)
                        bl_fillRect(sx, 1, x0, y0);
                if (sy > 1)
                        bl_fillRect(sx, 1, x0, y0 + sy - 1);
                if (sy > 2) {
                        bl_fillRect(1, sy - 2, x0, y0 + 1);
                        if (sx > 1)
                                bl_fillRect(1, sy - 2, x0 + sx - 1, y0 + 1);
                }
        }
        return;
}
