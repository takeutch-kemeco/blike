#include <blike0.h>

blMain()
{
        int x, y, k;
        bl_openWin(640, 384);
        bl_slctWin(1);
        bl_fillOval(1, 1, 1, 1);
        bl_fillOval(640 - 1, 384 - 1, 1, 1);
        bl_setCol(0x0000ff);
        bl_drawStr(192, 136, 8, 8, "%d", 1234);
        bl_copyRct0(640, 384, 1, 0, 0, 0, 0, 0);
        bl_slctWin(0);
        bl_flshWin(640, 384, 0, 0);
        x = 320; y = 192;
        bl_setCol(0xffff00);
        for (;;) {
                bl_fillOval(32, 32, x, y);
                bl_flshWin(32, 32, x, y);
                k = bl_inkey(BL_GETKEY | BL_WAITKEYNF);
                bl_copyRct0(32, 32, 1, x, y, 0, x, y);
                bl_flshWin(32, 32, x, y);
                if (k == KEY_LEFT && x > 0) x -= 32;
                if (k == KEY_RIGHT && x < 640 - 32) x += 32;
                if (k == KEY_UP && y > 0) y -= 32;
                if (k == KEY_DOWN && y < 384 - 32) y += 32;
        }
}
