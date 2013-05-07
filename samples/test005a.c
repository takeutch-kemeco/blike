#include "blike.h"

blMain()
{
        int x, y;
        openWin(256, 256);
        for (y = 0; y < 256; y++) {
                for (x = 0; x < 256; x++) {
                        setPix(x, y, bl_rgb(x, y, 0));
                }
        }
        locate(13, 8);
        setCol(iCol(7));
        printf("hello");
        wait(-1);
}
