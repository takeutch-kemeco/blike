#include "blikedrv.h"

#define w bl_work

/* このファイルは、テスト実装中の関数群が入る */

int *bl_getGrpB()
{
        BL_READY_WINDOW
        return w.win[BL_SLCTWIN].buf;
}
