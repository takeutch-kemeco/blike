#include <blike0.h>

blMain()
{
    for (;;) {
                if (bl_inkey(0) == 0) bl_wait(100);
                int i = bl_inkey(BL_WAITKEY | BL_GETKEY);
        //      int i = bl_inkey(BL_WAITKEY | BL_GETKEY | BL_DELFFF);
        //      bl_wait(10);
                bl_printf("%d ", i);
        }
        bl_wait(-1);
}
