#include <blike0.h>

blMain()
{
        int x, i;
        bl_openWin(320, 200);
        static int c[8] = {
                0x000000, 0xff0000, 0x00ff00, 0xffff00,
                0x0000ff, 0xff00ff, 0x00ffff, 0xffffff
        };
        for (i = 1; ; i = (i + 1) & 7) {
                bl_setCol(c[i]);
                for (x = 0; x < 320; x += 2) {
                        bl_fillRect(1, 200, x, 0);
                        bl_fillRect(1, 200, 319 - x, 0);
                        bl_wait(30);
                }
        }
}
