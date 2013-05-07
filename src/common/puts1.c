#include "blikedrv.h"

#define w bl_work

void bl_puts1(const char *s)
{
        bl_puts(s);
        bl_putc('\n');
        return;
}
