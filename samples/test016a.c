#include <blike0.h>

blMain()
{
        bl_fillOval(1, 1, 1, 1);
        bl_fillOval(640 - 1, 400 - 1, 1, 1);
        bl_setCol(0x0000ff);
        bl_drawStr(192, 136, 8, 8, "%d", 1234);
        bl_wait(-1);
}
