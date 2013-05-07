#include "blikedrv.h"

#define w bl_work

void bl_scroll16()
{
        return;
}

void bl_putc(int c)
{
        BL_READY_WINDOW
        BL_READY_FONTS
        if (w.cx + 1 >= w.csiz_x) {
                w.cx = 0;
                w.cy += 2;
        }
        if (w.cy + 2 >= w.csiz_y)
                bl_scroll16();
        if (c == '\r') {
                w.cx = 0;
        } else if (c == '\n') {
                w.cx = 0;
                w.cy += 2;
        } else if (c == '\t') {
                do {
                        bl_putchar8b(w.cx, w.cy + 0, ' ', w.col0, w.col1);
                        bl_putchar8b(w.cx, w.cy + 1, ' ' + 256, w.col0, w.col1);
                        w.cx++;
                } while (w.cx % w.tabsiz != 0);
        } else {
                bl_putchar8b(w.cx, w.cy + 0, c, w.col0, w.col1);
                bl_putchar8b(w.cx, w.cy + 1, c + 256, w.col0, w.col1);
                w.cx++;
        }
        return;
}
