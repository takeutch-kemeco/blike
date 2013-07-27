#include <blike0.h>

blMain()
{
        int x;
        bl_openWin(256, 256);
        bl_setCol(0xffff00);
        bl_setMode(BL_PXOR);
        for (x = 0; x < 256; x++) {
                bl_drawLine(0, 0, x,   255);
                bl_drawLine(x, 0, 255, 255);
        }
        bl_wait(-1);
}
