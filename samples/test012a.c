#include <blike0.h>

blMain()
{
        int j = 0;
        bl_wait(0);
        for (;;) {
                int i;
                bl_printf("\r%03d", j++);
                bl_flshWin(24, 16, 0, 0);
                for (i = 0; i < 10; i++)
                bl_waitNF(100);
        }
        bl_wait(-1);
}
