#include "blike.h"

blMain()
{
    int a, b, c, bb, cc, d;
        int old_b, old_c, old_d;
    cls();
    wait(0);
    b = 39; c = 11;
    bb = 1; cc = -1;
    d = 37;
        old_b = b;
        old_c = c;
        old_d = d;
    for (;;) {
        locate(b, c ); printf("O");
        locate(d, 23); printf("#####");
                flshWin(8, 16, b * 8, c * 16);
                flshWin(8 * 5, 16, d * 8, 23 * 16);
                flshWin(8, 16, old_b * 8, old_c * 16);
                flshWin(8 * 5, 16, old_d * 8, 23 * 16);
        if (c == 23) { break; }
        waitNF(100);
        a = inkey();
        locate(b, c ); printf(" ");
        locate(d, 23); printf("     ");
                old_b = b;      /* 描画更新がすべて終わってからflshするのが一番正しい */
                old_c = c;
                old_d = d;
        if (b ==  0) { bb =  1; }
        if (b == 78) { bb = -1; }
        if (c ==  0) { cc =  1; }
        if (c == 22) {
            if (d - 1 <= b && b <= d + 5) { cc = -1; }
        }
        b += bb; c += cc;
        if (a == 333 && d < 73) { d += 2; }
        if (a == 331 && d >  1) { d -= 2; }
        if (a == '6' && d < 73) { d += 2; }
        if (a == '4' && d >  1) { d -= 2; }
    }
        wait(-1);
}
