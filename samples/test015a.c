#include "blike.h"

blMain()
{
	int x, y, c, f, k;
	setBCol(0xffffff);
	openWin(256, 256);
	x = 0; y = 0; f = 1;
	for (;;) {
		if (f != 0) {
			setMode(BL_PXOR);
			setCol(0xffffff);
			drawRect(14, 14, x * 16 + 1, y * 16 + 1);
			flshWin(16, 16, x * 16, y * 16);
		}
		do {
			waitNF(10);
			k = inkey();
		} while (k == 0);
		if (f != 0) {
			drawRect(14, 14, x * 16 + 1, y * 16 + 1);
			setMode(BL_PSET);
			flshWin(16, 16, x * 16, y * 16);
		}
		c = -1;
		if (k == '4') k = KEY_LEFT;
		if (k == '6') k = KEY_RIGHT;
		if (k == '8') k = KEY_UP;
		if (k == '2') k = KEY_DOWN;
		if (k == KEY_LEFT  && x >  0) x--;
		if (k == KEY_RIGHT && x < 15) x++;
		if (k == KEY_UP    && y >  0) y--;
		if (k == KEY_DOWN  && y < 15) y++;
		if (k == ' ') f ^= 1;
		if (k == 'a') c = 0x000000;
		if (k == 's') c = 0x0000ff;
		if (k == 'd') c = 0x00ff00;
		if (k == 'f') c = 0x00ffff;
		if (k == 'g') c = 0xff0000;
		if (k == 'h') c = 0xff00ff;
		if (k == 'j') c = 0xffff00;
		if (k == 'k') c = 0xffffff;
		if (c >= 0) {
			setCol(c);
			fillRect(16, 16, x * 16, y * 16);
			flshWin(16, 16, x * 16, y * 16);
		}
	}
	wait(-1);
}
