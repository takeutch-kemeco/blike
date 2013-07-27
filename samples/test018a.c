#include <blike0.h>

blMain()
{
        int sc, x, y, vx, vy, ox, oy, k, x16, y16, t, lv;
        for (;;) {
                bl_setCol(0x00009f); bl_fillRect(320, 32, 160, 176);
                bl_setCol(0xffffff); bl_drawStr(160, 176, 2, 2, "Hit [Enter] to start");
                for (;;) {
                        if (bl_inkey(BL_WAITKEY | BL_GETKEY) == KEY_ENTER) break;
                }
                sc = 0;
                bl_srand(1);
                for (lv = 1;;) {
                        bl_cls();
                        bl_setMode(BL_PSET);
                        bl_setCol(0xffffff);
                        bl_fillRect(32, 32, 32, bl_rnd(176) + 160);
                        for (k = 0; k < 20; k++) {
                                bl_setCol(bl_iCol(bl_rnd(6) + 1));
                                bl_fillOval(lv * 8, lv * 8, bl_rnd(512 - lv * 8) + 64, bl_rnd(368 - lv * 8) + 32);
                        }
                        x = 630; y = 40; vx = 0; vy = 0; t = lv * 1000 + 990;
                        x16 = x * 16; y16 = y * 16;
                        bl_flshWin(999, 999, 0, 0);
                        for (;;) {
                                k = bl_getPix(x, y);
                                if (t % 10 == 0) {
                                        bl_setMode(BL_PSET);
                                        bl_setCol(0x000000);
                                        bl_fillRect(640, 32, 0, 0);
                                        bl_setCol(0xfefefe);
                                        bl_drawStr(0, 0, 2, 2, "SCORE=%05d  LV=%d  TIME=%03d", sc, lv, t / 10);
                                        bl_flshWin(640, 32, 0, 0);
                                }
                                bl_setMode(BL_PXOR);
                                bl_setCol(0xffffff);
                                bl_fillRect(3, 3, x - 2, y - 4);
                                bl_fillRect(5, 5, x - 3, y - 1);
                                bl_flshWin(5, 8, x - 3, y - 4);
                                bl_flshWin(5, 8, ox - 3, oy - 4);
                                if (k != 0x000000) break;
                                bl_waitNF(100);
                                k = bl_inkey1();
                                ox = x;
                                oy = y;
                                bl_fillRect(3, 3, x - 2, y - 4);
                                bl_fillRect(5, 5, x - 3, y - 1);
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
                                bl_setCol(0xff0000);
                                bl_setMode(BL_PSET);
                                for (k = 3; k < 24; k += 3) {
                                        bl_waitNF(200);
                                        bl_drawRect(k * 2, k * 2, x - k, y - k);
                                        bl_flshWin(k * 2, k * 2, x - k, y - k);
                                }
                                break;
                        }
                        sc += t / 10;
                        if (lv < 9) lv++;
                        bl_waitNF(2000);
                }
        }
}
