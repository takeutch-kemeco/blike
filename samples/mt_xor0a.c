#include <blike0.h>

blMain()
{
        int x, y, i;
        static int c[8] = {
                0x000000, 0xbf0000, 0x00bf00, 0xbfbf00,
                0x0000bf, 0xbf00bf, 0x00bfbf, 0xffffff
        };
        bl_openWin(160, 48);
        bl_setPix(49 + 30, 0, 0xc6c6c6);
        for (y = 0; y < 47; y++) {
                for (x = 1 + 30; x < 99 + 30; x++)
                        bl_setPix(x, y + 1, bl_getPix(x - 1, y) ^ bl_getPix(x + 1, y));
        }
        bl_setMode(BL_PXOR);
        bl_setCol(0xc6c6c6);
        bl_fillRect(160, 48, 0, 0);
        bl_setMode(BL_PSET);
        for (i = 0; ; i = (i + 1) & 7) {
                for (y = 0; y < 48; y++) {
                        for (x = 0; x < 160; x++) {
                                if (bl_getPix(x, y) != 0xc6c6c6)
                                        bl_setPix(x, y, c[i]);
                        }
                }
                bl_wait(500);
        }
}
