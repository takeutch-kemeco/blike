#include "blikedrv.h"

#define w bl_work

void bl_puts(const char *s)
{
        while (*s)
                bl_putc(*s++);
        return;
}
