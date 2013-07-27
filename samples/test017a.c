#include <blike0.h>

blMain()
{
        int sc, x, k, bx, by, t, osc, ox;
        bl_setBCol(0xffffff);
        bl_openWin(384, 384);
        for (;;) {
                bl_setCol(0x0000ff); bl_drawStr(32, 176, 2, 2, "Hit [Enter] to start");
                for (;;) {
                        if (bl_inkey(BL_WAITKEY | BL_GETKEY) == KEY_ENTER) break;
                }
                bl_cls();
                sc = 0; osc = 9; x = 176; ox = 0; by = -1; bx = 0; t = 0;
                bl_flshWin(384, 384, 0, 0);
                for (;;) {
                        if (sc != osc) {
                                bl_setCol(0xffffff); bl_fillRect(160, 32, 16, 352);
                                bl_setCol(0x000000); bl_drawStr(16, 352, 2, 2, "SCORE = %02d", sc);
                                bl_flshWin(160, 32, 16, 352);
                                osc = sc;
                        }
                        if (ox != x) {
                                bl_setCol(0xffffff); bl_fillRect(20, 8, ox - 2, 336);
                                if (sc > 0)
                                        bl_fillRect(16, (sc + 1) * 16, ox, 320 - sc * 16);
                                bl_setCol(0x0000ff); bl_fillRect(20, 8, x - 2, 336);
                                bl_setCol(0xb8860b);
                                for (k = 0; k < sc; k++)
                                        bl_fillOval(16, 16, x, 320 - k * 16);
                                bl_flshWin(20, 8, ox - 2, 336);
                                bl_flshWin(20, 8, x - 2, 336);
                                if (sc > 0) {
                                        bl_flshWin(16, sc * 16, ox, 336 - sc * 16);
                                        bl_flshWin(16, sc * 16, x, 336 - sc * 16);
                                }
                                ox = x;
                        }
                        if (by == -1) {
                                bx = bl_rnd(24) * 16;
                                by = 0;
                                bl_setCol(0xb8860b); bl_fillOval(16, 16, bx, by);
                                bl_flshWin(16, 16, bx, by);
                        } else {
                                bl_setCol(0xffffff); bl_fillRect(16, 16, bx, by - 16);
                                bl_setCol(0xb8860b); bl_fillOval(16, 16, bx, by);
                                bl_flshWin(16, 32, bx, by - 16);
                        }
                        if (by >= 336) break;
                        bl_waitNF(50);
                        k = bl_inkey1();
                        if (k == KEY_LEFT || k == '4') {
                                if (x > 0 && !(by > 320 - sc * 16 && bx == x - 16)) {
                                        x -= 16;
                                }
                        }
                        if (k == KEY_RIGHT || k == '6') {
                                if (x < 368 && !(by > 320 - sc * 16 && bx == x + 16)) {
                                        x += 16;
                                }
                        }
                        if (bx == x && 320 - sc * 16 == by) {
                                by = -1;
                                sc++;
                        }
                        t++;
                        if (t >= 4) {
                                by += 16;
                                t = 0;
                        }
                }
        }
}
