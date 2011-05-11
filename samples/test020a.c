#include "blike.h"

blMain()
{
	int x, y, k;
	openWin(640, 384);
	slctWin(1);
	fillOval(1, 1, 1, 1);
	fillOval(640 - 1, 384 - 1, 1, 1);
	setCol(0x0000ff);
	drawStr(192, 136, 8, 8, "%d", 1234);
	copyRct0(640, 384, 1, 0, 0, 0, 0, 0);
	slctWin(0);
	flshWin(640, 384, 0, 0);
	x = 320; y = 192;
	setCol(0xffff00);
	for (;;) {
		fillOval(32, 32, x, y);
		flshWin(32, 32, x, y);
		k = bl_inkey(BL_GETKEY | BL_WAITKEYNF);
		copyRct0(32, 32, 1, x, y, 0, x, y);
		flshWin(32, 32, x, y);
		if (k == KEY_LEFT && x > 0) x -= 32;
		if (k == KEY_RIGHT && x < 640 - 32) x += 32;
		if (k == KEY_UP && y > 0) y -= 32;
		if (k == KEY_DOWN && y < 384 - 32) y += 32;
	}
}
