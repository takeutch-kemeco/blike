#include "blike.h"

blMain()
{
	openWin(256, 256);
	
	while(1) {
		setCol(0xffff00);
		bl_draw_2db_spline(
			0,   100,
			100,   0,
			255, 100
		);

		wait(1000/60);
	}
}
