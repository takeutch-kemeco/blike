#include <blike0.h>

blMain()
{
        int a;
        for (a = 0; a <= 15; a++) {
                bl_color(a, 0);
                bl_printf("color : %2d\n", a);
        }
        bl_wait(-1);
}
