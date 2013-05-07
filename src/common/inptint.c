#include "blikedrv.h"

#define w bl_work

int bl_inptInt(const char *s, ...)
{
        char buf[64 * 1024];
        int r;
        va_list ap;
        va_start(ap, s);
        bld_vsnprintf(buf, (sizeof buf) - 1, s, ap);
        va_end(ap);
        bl_puts((unsigned char *) buf);
        bl_scanf("%d", &r);
        return r;
}
