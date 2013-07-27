#include <blike0.h>

blMain()
{
        int x, y;
        bl_openWin(256, 256);
        for (y = 0; y < 256; y++) {
                for (x = 0; x < 256; x++) {
                        bl_setPix(x, y, bl_rgb(x, y, 0));
                }
        }
        bl_locate(13, 8);
        bl_setCol(bl_iCol(7));
        bl_printf("hello");
        bl_wait(-1);
}
