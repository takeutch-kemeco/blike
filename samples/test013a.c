#include <blike0.h>

blMain()
{
        int a, b, c, bb, cc, d;
        bl_cls();
        bl_wait(0);
        b = 39; c = 11;
        bb = 1; cc = -1;
        d = 37;
        for (;;) {
                bl_locate(b, c ); bl_printf("O");
                bl_locate(d, 23); bl_printf("#####");
                bl_flshWin(8, 16, b * 8, c * 16);
                bl_flshWin(8 * 5, 16, d * 8, 23 * 16);
                if (c == 23) { break; }
                bl_waitNF(100);
                a = bl_inkey1();
                bl_locate(b, c ); bl_printf(" ");
                bl_locate(d, 23); bl_printf("     ");
                bl_flshWin(8, 16, b * 8, c * 16);
                bl_flshWin(8 * 5, 16, d * 8, 23 * 16);
                if (b ==  0) { bb =  1; }
                if (b == 78) { bb = -1; }
                if (c ==  0) { cc =  1; }
                if (c == 22) {
                        if (d - 1 <= b && b <= d + 5) { cc = -1; }
                }
                b += bb; c += cc;
                if (a == KEY_RIGHT && d < 73) { d += 2; }
                if (a == KEY_LEFT  && d >  1) { d -= 2; }
                if (a == '6'       && d < 73) { d += 2; }
                if (a == '4'       && d >  1) { d -= 2; }
        }
        bl_wait(-1);
}
