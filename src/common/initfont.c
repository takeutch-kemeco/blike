#include "blikedrv.h"

#define w bl_work

void bl_initFont()
{
        int i;
        for (i = 0; i < 256; i++) {
                w.ftyp[i] = 1;
                w.fptn[i] = hankaku + 16 * i;
                w.ftyp[i + 256] = 1;
                w.fptn[i + 256] = hankaku + 16 * i + 8;
        }
        return;
}
