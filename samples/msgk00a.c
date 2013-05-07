#include "blike.h"

#define WIDTH 60

blMain()
{
        int i, j;
        static char msg[5] = "OSASK";
        openWin(WIDTH * 8, 16);
        setCol(0xffff00);
        for (;;) {
                for (i = 0; i < sizeof msg; i++) {
                        for (j = WIDTH; i < j; ) {
                                if (j < WIDTH) {
                                        locate(j, 0);
                                        putc(' ');
                                }
                                j--;
                                locate(j, 0);
                                putc(msg[i]);
                                wait(100);
                        }
                        wait(1000);
                }
                wait(5000);
                cls();
        }
}
