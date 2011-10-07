#include "blikedrv.h"

#define	w	bl_work

void bl_gets(char *s)
/* せいぜい1024バイトしか入力できない, 改行は残る */
{
	int mod0 = w.mod, x0 = w.cx, c, l = 0, i;
	bl_setMode(BL_PXOR);
	s[0] = '¥0';
	for (;;) {
		bl_fillRect(2, 16, w.cx * 8, w.cy * 16);
		c = bl_inkey(BL_WAITKEY | BL_GETKEY | BL_DELFFF);
		bl_fillRect(2, 16, w.cx * 8, w.cy * 16);
		if (' ' <= c && c <= 0x7e && w.cx < w.csiz_x - 1) {
			l++;
			for (i = l; i > w.cx - x0; i--)
				s[i] = s[i - 1];
			s[w.cx - x0] = c;
		}
		if (c == KEY_LEFT && w.cx > x0)
			w.cx--;
		if (c == KEY_RIGHT && w.cx - x0 < l)
			w.cx++;
		if (c == KEY_BACKSPACE && w.cx - x0 > 0) {
			l--;
			i = w.cx;
			bl_locate(x0 + l, w.cy);
			bl_putc(' ');
			w.cx = i - 1;
		//	w.cx--;
			for (i = w.cx - x0; i <= l; i++)
				s[i] = s[i + 1];
		}
		if (c == KEY_DEL && w.cx - x0 < l && l > 0) {
			l--;
			i = w.cx;
			bl_locate(x0 + l, w.cy);
			bl_putc(' ');
			w.cx = i;
			for (i = w.cx - x0; i <= l; i++)
				s[i] = s[i + 1];
		}
		if (c == KEY_ENTER)
			break;
		bl_locate(x0, w.cy);
		bl_puts(s);
	}
	w.mod = mod0;
	bl_putc('¥n');
	return;
}