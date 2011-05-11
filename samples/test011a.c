#include "blike.h"

blMain()
{
	char s[1024];
	gets(s);
	printf("!%s!\n", s);
	wait(-1);
}
