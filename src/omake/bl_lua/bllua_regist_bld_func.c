#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>

static int bllua_func_bld_openWin(lua_State* a)
{
	int x = luaL_checkint(a, -2);
	int y = luaL_checkint(a, -1);
	bld_openWin(x, y);

	return 1;
}

static int bllua_func_bld_flshWin(lua_State* a)
{
	int sx = luaL_checkint(a, -4);
	int sy = luaL_checkint(a, -3);
	int x0 = luaL_checkint(a, -2);
	int y0 = luaL_checkint(a, -1);
	bld_flshWin(sx, sy, x0, y0);

	return 1;
}

static int bllua_func_bld_flshSys(lua_State* a)
{
	bld_flshSys();

	return 1;
}

static int bllua_func_bld_waitNF(lua_State* a)
{
	bld_waitNF();

	return 1;
}

static int bllua_func_bld_exit(lua_State* a)
{
	bld_exit();

	return 1;
}

static int bllua_func_bld_getSeed(lua_State* a)
{
	int tmp = bld_getSeed();
	lua_pushnumber(a, tmp);

	return 1;
}

static int bllua_func_bld_malloc(lua_State* a)
{
	return 1;
}

static int bllua_func_bld_free(lua_State* a)
{
	return 1;
}

static int bllua_func_bld_initFont(lua_State* a)
{
	bld_initFont();

	return 1;
}

static int bllua_func_bld_maxfonts(lua_State* a)
{
	int tmp = bld_maxfonts();
	lua_pushnumber(a, tmp);

	return 1;
}

static int bllua_func_bld_vsnprintf(lua_State* a)
{
	//よくわからないので保留
	
//	int tmp = bld_vsnprintf(char *b, int n, const char *f, va_list ap);
	int tmp = 0;
	lua_pushnumber(a, tmp);

	return 1;
}

static int bllua_func_bld_lock(lua_State* a)
{
	bld_lock();

	return 1;
}

static int bllua_func_bld_unlock(lua_State* a)
{
	bld_unlock();

	return 1;
}

void bllua_regist_bld_func(lua_State* a)
{
	lua_register(a, "bld_openWin", bllua_func_bld_openWin);
	lua_register(a, "bld_flshWin", bllua_func_bld_flshWin);
	lua_register(a, "bld_flshSys", bllua_func_bld_flshSys);
	lua_register(a, "bld_waitNF", bllua_func_bld_waitNF);
	lua_register(a, "bld_exit", bllua_func_bld_exit);
	lua_register(a, "bld_getSeed", bllua_func_bld_getSeed);
	lua_register(a, "bld_malloc", bllua_func_bld_malloc);
	lua_register(a, "bld_free", bllua_func_bld_free);
	lua_register(a, "bld_initFont", bllua_func_bld_initFont);
	lua_register(a, "bld_maxfonts", bllua_func_bld_maxfonts);
	lua_register(a, "bld_vsnprintf", bllua_func_bld_vsnprintf);
	lua_register(a, "bld_lock", bllua_func_bld_lock);
	lua_register(a, "bld_unlock", bllua_func_bld_unlock);

	return 1;
}
