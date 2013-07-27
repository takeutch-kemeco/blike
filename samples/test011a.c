#include <blike0.h>

blMain()
{
        bl_openWin(320, 320);

        char s[1024];

        while (1) {
                bl_gets(s);
                bl_printf("!%s!\n", s);
        }

        bl_wait(-1);
}

