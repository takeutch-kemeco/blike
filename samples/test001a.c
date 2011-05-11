#include "blike.h"

blMain()
{
	int a;
	for (a = 0; a <= 15; a++) {
		color(a, 0);
		printf("color : %2d\n", a);
	}
	wait(-1);
}
