#include "blikedrv.h"

#define	w	bl_work

void bl_drawStr(int x0, int y0, int rx, int ry, const char *s, ...)
{
	unsigned char buf[64 * 1024];
	int i, x, y, c;
	unsigned char p;
	va_list ap;
	va_start(ap, s);
	bld_vsnprintf(buf, (sizeof buf) - 1, s, ap);
	va_end(ap);
	BL_READY_FONTS
	if (rx <= 0 || ry <= 0) return;
	for (i = 0; (c = buf[i]) != '\0'; i++) {
		for (y = 0; y < 16; y++) {
			if (y < 8)
				p = w.fptn[c][y];
			else
				p = w.fptn[c + 256][y - 8];
			for (x = 0; x < 8; x++) {
				if ((p & (0x80 >> x)) != 0)
					bl_fillRect(rx, ry, x0 + (i * 8 + x) * rx, y0 + y * ry);
			}
		}
	}
	return;
}
