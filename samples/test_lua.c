#include "blike.h"

extern int bllua_run(char* file_name);

blMain()
{
	bllua_run("test_lua.lua");
	while(1) {
		bl_wait(1);
	}
}

