#include "blikedrv.h"

#define w bl_work

int bl_iCol(int i)
{
        static int t[] = {
                0x000000, 0x0000ff, 0x00ff00, 0x00ffff, 0xff0000, 0xff00ff, 0xffff00, 0xffffff,
                0x5f5f5f, 0x0000af, 0x00af00, 0x00afaf, 0xaf0000, 0xaf00af, 0xafaf00, 0xafafaf
        };
        if (0 <= i && i <= 15)
                return t[i];
        return -1;
}
