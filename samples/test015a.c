#include <blike0.h>

blMain()
{
        int x, y, c, f, k;
        bl_setBCol(0xffffff);
        bl_openWin(256, 256);
        x = 0; y = 0; f = 1;
        for (;;) {
                if (f != 0) {
                        bl_setMode(BL_PXOR);
                        bl_setCol(0xffffff);
                        bl_drawRect(14, 14, x * 16 + 1, y * 16 + 1);
                        bl_flshWin(16, 16, x * 16, y * 16);
                }
                do {
                        bl_waitNF(10);
                        k = bl_inkey1();
                } while (k == 0);
                if (f != 0) {
                        bl_drawRect(14, 14, x * 16 + 1, y * 16 + 1);
                        bl_setMode(BL_PSET);
                        bl_flshWin(16, 16, x * 16, y * 16);
                }
                c = -1;
                if (k == '4') k = KEY_LEFT;
                if (k == '6') k = KEY_RIGHT;
                if (k == '8') k = KEY_UP;
                if (k == '2') k = KEY_DOWN;
                if (k == KEY_LEFT  && x >  0) x--;
                if (k == KEY_RIGHT && x < 15) x++;
                if (k == KEY_UP    && y >  0) y--;
                if (k == KEY_DOWN  && y < 15) y++;
                if (k == ' ') f ^= 1;
                if (k == 'a') c = 0x000000;
                if (k == 's') c = 0x0000ff;
                if (k == 'd') c = 0x00ff00;
                if (k == 'f') c = 0x00ffff;
                if (k == 'g') c = 0xff0000;
                if (k == 'h') c = 0xff00ff;
                if (k == 'j') c = 0xffff00;
                if (k == 'k') c = 0xffffff;
                if (c >= 0) {
                        bl_setCol(c);
                        bl_fillRect(16, 16, x * 16, y * 16);
                        bl_flshWin(16, 16, x * 16, y * 16);
                }
        }
        bl_wait(-1);
}
