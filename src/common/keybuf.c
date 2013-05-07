#include "blikedrv.h"

#define w bl_work

void bl_putKeyB(int n, int *p)
{
        bld_lock();
        if (w.kbuf_c + n <= BL_SIZ_KBUF - 2) {
                int i;
                w.kbuf_c += n;
                for (i = 0; i < n; i++) {
                        w.kbuf[w.kbuf_wp] = p[i];
                        w.kbuf_wp = (w.kbuf_wp + 1) & ~(BL_SIZ_KBUF - 1);
                }
        }
        bld_unlock();
        return;
}
