#include "blikedrv.h"

#define w bl_work

void bl_color(int c, int b)
/* -1を指定すれば変更しない */
{
        if (0 <= c && c <= 15)
                bl_setCol(bl_iCol(c));
        if (0 <= b && b <= 15)
                bl_setBCol(bl_iCol(b));
        return;
}
