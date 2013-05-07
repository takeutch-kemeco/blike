#include "blike.h"

blMain()
{
        int x;
        openWin(256, 256);
        setCol(0xffff00);
        setMode(BL_PXOR);
        for (x = 0; x < 256; x++) {
                drawLine(0, 0, x,   255);
                drawLine(x, 0, 255, 255);
        }
        wait(-1);
}
