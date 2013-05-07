#include "blikedrv.h"

#define w bl_work

void bl_setBCol(int c)
{
//      if (0 <= c && c <= 0xffffff)
                w.col1 = c;
        return;
}

