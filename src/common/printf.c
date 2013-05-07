#include "blikedrv.h"

#define w bl_work

void bl_printf(const char *s, ...)
{
        char buf[64 * 1024];
        va_list ap;
        va_start(ap, s);
        bld_vsnprintf(buf, (sizeof buf) - 1, s, ap);
        va_end(ap);
        bl_puts((unsigned char *) buf);
        return;
}
