#include "blike.h"

blMain()
{
        fillOval(1, 1, 1, 1);
        fillOval(640 - 1, 400 - 1, 1, 1);
        setCol(0x0000ff);
        drawStr(192, 136, 8, 8, "%d", 1234);
        wait(-1);
}
