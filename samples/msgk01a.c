#include "blike.h"

#define	WIDTH			60

blMain()
{
	int i;
	char tmp;
	static char msg[WIDTH + 1] = "OSASK";
	openWin(WIDTH * 8, 16);
	setCol(0xffff00);
	for (i = 0; i < WIDTH; i++) {
		if (msg[i] == '\0')
			msg[i] = ' ';
	}
	msg[WIDTH] = '\0';
	for (;;) {
		locate(0, 0);
		puts(msg);
		tmp = msg[0];
		for (i = 0; i < WIDTH - 1; i++)
			msg[i] = msg[i + 1];
		msg[WIDTH - 1] = tmp;
		wait(100);
	}
}
