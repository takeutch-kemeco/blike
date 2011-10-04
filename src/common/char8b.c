#include "blikedrv.h"

#define	w	bl_work

void setPix0(int x, int y, int c)
{
	if (0 <= x && x < w.win[BL_SLCTWIN].xsiz && 0 <= y && y < w.win[BL_SLCTWIN].xsiz)
		w.win[BL_SLCTWIN].buf[x + y * w.win[BL_SLCTWIN].xsiz] = c;
	return;
}

void bl_putchar8b(int x, int y, int n, int c, int b)
{
	int i;
	unsigned char *p = w.fptn[n];
	if (0 <= x && x <= w.csiz_x && 0 <= y && y <= w.csiz_y) {
		w.cbuf[x + y * w.csiz_x] = n;
		w.ccol[x + y * w.csiz_x] = c;
		w.cbak[x + y * w.csiz_x] = b;
		for (i = 0; i < 8; i++) {
			if ((p[i] & 0x80) != 0) setPix0(x * 8 + 0, y * 8 + i, c); else setPix0(x * 8 + 0, y * 8 + i, b);
			if ((p[i] & 0x40) != 0) setPix0(x * 8 + 1, y * 8 + i, c); else setPix0(x * 8 + 1, y * 8 + i, b);
			if ((p[i] & 0x20) != 0) setPix0(x * 8 + 2, y * 8 + i, c); else setPix0(x * 8 + 2, y * 8 + i, b);
			if ((p[i] & 0x10) != 0) setPix0(x * 8 + 3, y * 8 + i, c); else setPix0(x * 8 + 3, y * 8 + i, b);
			if ((p[i] & 0x08) != 0) setPix0(x * 8 + 4, y * 8 + i, c); else setPix0(x * 8 + 4, y * 8 + i, b);
			if ((p[i] & 0x04) != 0) setPix0(x * 8 + 5, y * 8 + i, c); else setPix0(x * 8 + 5, y * 8 + i, b);
			if ((p[i] & 0x02) != 0) setPix0(x * 8 + 6, y * 8 + i, c); else setPix0(x * 8 + 6, y * 8 + i, b);
			if ((p[i] & 0x01) != 0) setPix0(x * 8 + 7, y * 8 + i, c); else setPix0(x * 8 + 7, y * 8 + i, b);
		}
	}
}
