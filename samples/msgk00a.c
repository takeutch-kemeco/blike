#include <blike0.h>

#define WIDTH 60

blMain()
{
        int i, j;
        static char msg[5] = "OSASK";
        bl_openWin(WIDTH * 8, 16);
        bl_setCol(0xffff00);
        for (;;) {
                for (i = 0; i < sizeof msg; i++) {
                        for (j = WIDTH; i < j; ) {
                                if (j < WIDTH) {
                                        bl_locate(j, 0);
                                        bl_putc(' ');
                                }
                                j--;
                                bl_locate(j, 0);
                                bl_putc(msg[i]);
                                bl_wait(100);
                        }
                        bl_wait(1000);
                }
                bl_wait(5000);
                bl_cls();
        }
}

