#include <blike0.h>

blMain()
{
        bl_printf("argc = %d\n", bl_argc);

        int i;
        for(i = 0; i < bl_argc; i++) {
                bl_printf("[%d], %s\n", i, bl_argv[i]);
        }

        bl_wait(-1);
}

