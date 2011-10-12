#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include "bllua_regist_bld_func.h"

static lua_State* bllua_new_state(void)
{
	lua_State* a = luaL_newstate();
	luaL_openlibs(a);

	return a;
}

static void bllua_free_state(lua_State* a)
{
	lua_close(a);
}

int bllua_run(char* file_name)
{
	lua_State* a = bllua_new_state();
	
	bllua_regist_bld_func(a);
	bllua_regist_bl_func(a);
	
	if(luaL_loadfile(a, file_name)) {
		return 1;
	}

	if(lua_pcall(a, 0, 0, 0)) {
		return 2;
	}

	bllua_free_state(a);

	return 0;
}
