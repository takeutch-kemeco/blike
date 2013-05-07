#include "blike.h"

blMain()
{
        int sc, x, y, vx, vy, ox, oy, k, x16, y16, t, lv;
        for (;;) {
                setCol(0x00009f); fillRect(320, 32, 160, 176);
                setCol(0xffffff); drawStr(160, 176, 2, 2, "Hit [Enter] to start");
                for (;;) {
                        if (bl_inkey(BL_WAITKEY | BL_GETKEY) == KEY_ENTER) break;
                }
                sc = 0;
                bl_srand(1);
                for (lv = 1;;) {
                        cls();
                        setMode(BL_PSET);
                        setCol(0xffffff);
                        fillRect(32, 32, 32, rnd(176) + 160);
                        for (k = 0; k < 20; k++) {
                                setCol(iCol(rnd(6) + 1));
                                fillOval(lv * 8, lv * 8, rnd(512 - lv * 8) + 64, rnd(368 - lv * 8) + 32);
                        }
                        x = 630; y = 40; vx = 0; vy = 0; t = lv * 1000 + 990;
                        x16 = x * 16; y16 = y * 16;
                        flshWin(999, 999, 0, 0);
                        for (;;) {
                                k = getPix(x, y);
                                if (t % 10 == 0) {
                                        setMode(BL_PSET);
                                        setCol(0x000000);
                                        fillRect(640, 32, 0, 0);
                                        setCol(0xfefefe);
                                        drawStr(0, 0, 2, 2, "SCORE=%05d  LV=%d  TIME=%03d", sc, lv, t / 10);
                                        flshWin(640, 32, 0, 0);
                                }
                                setMode(BL_PXOR);
                                setCol(0xffffff);
                                fillRect(3, 3, x - 2, y - 4);
                                fillRect(5, 5, x - 3, y - 1);
                                flshWin(5, 8, x - 3, y - 4);
                                flshWin(5, 8, ox - 3, oy - 4);
                                if (k != 0x000000) break;
                                waitNF(100);
                                k = inkey();
                                ox = x;
                                oy = y;
                                fillRect(3, 3, x - 2, y - 4);
                                fillRect(5, 5, x - 3, y - 1);
                                if ((k == '2' || k == KEY_DOWN) && vy > -128)
                                        vy -= 4;
                                if ((k == '4' || k == KEY_LEFT) && vx < +128)
                                        vx += 4;
                                if ((k == '6' || k == KEY_RIGHT) && vx > -128)
                                        vx -= 4;
                                x16 += vx;
                                y16 += vy;
                                x = x16 >> 4;
                                y = y16 >> 4;
                                vy++;
                                if (t > 0) t--;
                        }
                        if (k != 0xffffff) {
                                setCol(0xff0000);
                                setMode(BL_PSET);
                                for (k = 3; k < 24; k += 3) {
                                        waitNF(200);
                                        drawRect(k * 2, k * 2, x - k, y - k);
                                        flshWin(k * 2, k * 2, x - k, y - k);
                                }
                                break;
                        }
                        sc += t / 10;
                        if (lv < 9) lv++;
                        waitNF(2000);
                }
        }
}
