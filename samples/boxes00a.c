#include <blike0.h>

blMain()
{
        int i;
        bl_setCol(0x0000ff);
        bl_setBCol(0xc6c6c6);
        bl_openWin(160, 144);
        for (i = 0; i <= 60; i += 3)
                bl_drawRect(19 + i * 2, 139 - i * 2, 80 - 9 - i, 72 - 69 + i);
        bl_wait(-1);
}
