#include "blike.h"

blMain()
{
        int j = 0;
        wait(0);
    for (;;) {
                int i;
                printf("\r%03d", j++);
                flshWin(24, 16, 0, 0);
                for (i = 0; i < 10; i++)
                waitNF(100);
        }
        wait(-1);
}
