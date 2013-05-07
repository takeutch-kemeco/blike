#include "blike.h"

blMain()
{
        int i;
        setCol(0x0000ff);
        setBCol(0xc6c6c6);
        openWin(160, 144);
        for (i = 0; i <= 60; i += 3)
                drawRect(19 + i * 2, 139 - i * 2, 80 - 9 - i, 72 - 69 + i);
        wait(-1);
}
