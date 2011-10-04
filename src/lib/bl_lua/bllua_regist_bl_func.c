#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include "blike0.h"

static int bllua_func_bl_openWin(lua_State* a)
{
	int x = luaL_checkint(a, -2);
	int y = luaL_checkint(a, -1);
	bl_openWin(x, y);

	return 1;
}

static int bllua_func_bl_putc(lua_State* a)
{
	int c = luaL_checkint(a, -1);
	bl_putc(c);

	return 1;
}

static int bllua_func_bl_puts(lua_State* a)
{
	const char* s = luaL_checkstring(a, -1);
	bl_puts(s);

	return 1;
}

static int bllua_func_bl_puts1(lua_State* a)
{
	const char* s = luaL_checkstring(a, -1);
	bl_puts1(s);

	return 1;
}

static int bllua_func_bl_setCol(lua_State* a)
{
	int c = luaL_checkint(a, -1);
	bl_setCol(c);

	return 1;
}

static int bllua_func_bl_setBCol(lua_State* a)
{
	int c = luaL_checkint(a, -1);
	bl_setBCol(c);

	return 1;
}

static int bllua_func_bl_rgb(lua_State* a)
{
	int r = luaL_checkint(a, -3);
	int g = luaL_checkint(a, -2);
	int b = luaL_checkint(a, -1);
	bl_rgb(r, g, b);

	return 1;
}

static int bllua_func_bl_iCol(lua_State* a)
{
	int i = luaL_checkint(a, -1);
	int tmp = bl_iCol(i);
	lua_pushnumber(a, tmp);

	return 1;
}

static int bllua_func_bl_flshWin(lua_State* a)
{
	int sx = luaL_checkint(a, -4);
	int sy = luaL_checkint(a, -3);
	int x0 = luaL_checkint(a, -2);
	int y0 = luaL_checkint(a, -1);
	bl_flshWin(sx, sy, x0, y0);

	return 1;
}

static int bllua_func_bl_getGrpB(lua_State* a)
{
	// よくわからないので保留

	return 1;
}

static int bllua_func_bl_setPix(lua_State* a)
{
	int x = luaL_checkint(a, -3);
	int y = luaL_checkint(a, -2);
	int c = luaL_checkint(a, -1);
	bl_setPix(x, y, c);

	return 1;
}

static int bllua_func_bl_fillRect(lua_State* a)
{
	int sx = luaL_checkint(a, -4);
	int sy = luaL_checkint(a, -3);
	int x0 = luaL_checkint(a, -2);
	int y0 = luaL_checkint(a, -1);
	bl_fillRect(sx, sy, x0, y0);

	return 1;
}

static int bllua_func_bl_drawRect(lua_State* a)
{
	int sx = luaL_checkint(a, -4);
	int sy = luaL_checkint(a, -3);
	int x0 = luaL_checkint(a, -2);
	int y0 = luaL_checkint(a, -1);
	bl_drawRect(sx, sy, x0, y0);

	return 1;
}

static int bllua_func_bl_drawLine(lua_State* a)
{
	int x0 = luaL_checkint(a, -4);
	int y0 = luaL_checkint(a, -3);
	int x1 = luaL_checkint(a, -2);
	int y1 = luaL_checkint(a, -1);
	bl_drawLine(x0, y0, x1, y1);

	return 1;
}

static int bllua_func_bl_rnd(lua_State* a)
{
	int max_plus_1 = luaL_checkint(a, -1);
	int tmp = bl_rnd(max_plus_1);
	lua_pushnumber(a, tmp);

	return 1;
}

static int bllua_func_bl_wait(lua_State* a)
{
	int msec = luaL_checkint(a, -1);
	bl_wait(msec);

	return 1;
}

static int bllua_func_bl_color(lua_State* a)
{
	int c = luaL_checkint(a, -2);
	int b = luaL_checkint(a, -1);
	bl_color(c, b);

	return 1;
}

static int bllua_func_bl_locate(lua_State* a)
{
	int x = luaL_checkint(a, -2);
	int y = luaL_checkint(a, -1);
	bl_locate(x, y);

	return 1;
}

static int bllua_func_bl_printf(lua_State* a)
{
	// よくわからないので保留
	
	return 1;
}

static int bllua_func_bl_getPix(lua_State* a)
{
	int x = luaL_checkint(a, -2);
	int y = luaL_checkint(a, -1);
	bl_getPix(x, y);

	return 1;
}

static int bllua_func_bl_waitNF(lua_State* a)
{
	int msec = luaL_checkint(a, -1);
	bl_waitNF(msec);

	return 1;
}

static int bllua_func_bl_inkey1(lua_State* a)
{
	int tmp = bl_inkey1();
	lua_pushnumber(a, tmp);

	return 1;
}

static int bllua_func_bl_cls(lua_State* a)
{
	bl_cls();

	return 1;
}

static int bllua_func_bl_clock(lua_State* a)
{
	bl_clock();

	return 1;
}

static int bllua_func_bl_inputInt(lua_State* a)
{
	// よくわからないので保留

	return 1;
}

static int bllua_func_bl_inputFloat(lua_State* a)
{
	// よくわからないので保留

	return 1;
}

static int bllua_func_bl_scanf(lua_State* a)
{
	// よくわからないので保留

	return 1;
}

static int bllua_func_bl_vscanf(lua_State* a)
{
	// よくわからないので保留

	return 1;
}

static int bllua_func_bl_setMode(lua_State* a)
{
	int mode = luaL_checkint(a, -1);
	bl_setMode(mode);

	return 1;
}

static int bllua_func_bl_rand(lua_State* a)
{
	int tmp = bl_rand();
	lua_pushnumber(a, tmp);

	return 1;
}

static int bllua_func_bl_srand(lua_State* a)
{
	int seed = luaL_checkint(a, -1);
	bl_srand(seed);

	return 1;
}

static int bllua_func_bl_inkey(lua_State* a)
{
	int flags = luaL_checkint(a, -1);
	int tmp = bl_inkey(flags);
	lua_pushnumber(a, tmp);

	return 1;
}

static int bllua_func_bl_gets(lua_State* a)
{
	// よくわからないので保留

	return 1;
}

static int bllua_func_bl_fillOval(lua_State* a)
{
	int sx = luaL_checkint(a, -4);
	int sy = luaL_checkint(a, -3);
	int x0 = luaL_checkint(a, -2);
	int y0 = luaL_checkint(a, -1);
	bl_fillOval(sx, sy, x0, y0);

	return 1;
}

static int bllua_func_bl_drawStr(lua_State* a)
{
	// よくわからないので保留

	return 1;
}

static int bllua_func_bl_openVWin(lua_State* a)
{
	int n = luaL_checkint(a, -3);
	int x = luaL_checkint(a, -2);
	int y = luaL_checkint(a, -1);
	bl_openVWin(n, x, y);

	return 1;
}

static int bllua_func_bl_slctWin(lua_State* a)
{
	int n = luaL_checkint(a, -1);
	bl_slctWin(n);

	return 1;
}

static int bllua_func_bl_copyRct0(lua_State* a)
{
	int sx = luaL_checkint(a, -8);
	int sy = luaL_checkint(a, -7);
	int n0 = luaL_checkint(a, -6);
	int x0 = luaL_checkint(a, -5);
	int y0 = luaL_checkint(a, -4);
	int n1 = luaL_checkint(a, -3);
	int x1 = luaL_checkint(a, -2);
	int y1 = luaL_checkint(a, -1);
	bl_copyRct0(sx, sy, n0, x0, y0, n1, x1, y1);

	return 1;
}

static int bllua_func_bl_copyRct1(lua_State* a)
{
	int sx = luaL_checkint(a, -9);
	int sy = luaL_checkint(a, -8);
	int n0 = luaL_checkint(a, -7);
	int x0 = luaL_checkint(a, -6);
	int y0 = luaL_checkint(a, -5);
	int n1 = luaL_checkint(a, -4);
	int x1 = luaL_checkint(a, -3);
	int y1 = luaL_checkint(a, -2);
	int ic = luaL_checkint(a, -1);
	bl_copyRct1(sx, sy, n0, x0, y0, n1, x1, y1, ic);

	return 1;
}

static int bllua_func_bl_drawPtrn_r(lua_State* a)
{
	int sx = luaL_checkint(a, -6);
	int sy = luaL_checkint(a, -5);
	int x0 = luaL_checkint(a, -4);
	int y0 = luaL_checkint(a, -3);
	const char* c = luaL_checkstring(a, -2);
	const char* p = luaL_checkstring(a, -1);
	bl_drawPtrn_r(sx, sy, x0, y0, c, p);

	return 1;
}

static int bllua_func_bl_drawPtrn_d(lua_State* a)
{
	int sx = luaL_checkint(a, -6);
	int sy = luaL_checkint(a, -5);
	int x0 = luaL_checkint(a, -4);
	int y0 = luaL_checkint(a, -3);
	const char* c = luaL_checkstring(a, -2);
	const char* p = luaL_checkstring(a, -1);
	bl_drawPtrn_d(sx, sy, x0, y0, c, p);

	return 1;
}

static int bllua_func_bl_readyWin(lua_State* a)
{
	int n = luaL_checkint(a, -1);
	bl_readyWin(n);

	return 1;
}

static int bllua_func_bl_setPtrn0(lua_State* a)
{
	// よくわからないので保留

	return 1;
}

static int bllua_func_bl_drawPtrn_err_r(lua_State* a)
{
	const char* msg = luaL_checkstring(a, -2);
	unsigned char* nam = (unsigned char*)luaL_checkstring(a, -1);
	bl_drawPtrn_err_r(msg, nam);

	return 1;
}

static int bllua_func_bl_drawPtrn_err_d(lua_State* a)
{
	const char* msg = luaL_checkstring(a, -2);
	unsigned char* nam = (unsigned char*)luaL_checkstring(a, -1);
	bl_drawPtrn_err_d(msg, nam);

	return 1;
}

static int bllua_func_KEY_ENTER(lua_State* a)
{
	lua_pushnumber(a, KEY_ENTER);

	return 1;
}

static int bllua_func_KEY_ESC(lua_State* a)
{
	lua_pushnumber(a, KEY_ESC);

	return 1;
}

static int bllua_func_KEY_BACKSPACE(lua_State* a)
{
	lua_pushnumber(a, KEY_BACKSPACE);

	return 1;
}

static int bllua_func_KEY_TAB(lua_State* a)
{
	lua_pushnumber(a, KEY_TAB);

	return 1;
}

static int bllua_func_KEY_LEFT(lua_State* a)
{
	lua_pushnumber(a, KEY_LEFT);

	return 1;
}

static int bllua_func_KEY_RIGHT(lua_State* a)
{
	lua_pushnumber(a, KEY_RIGHT);

	return 1;
}

static int bllua_func_KEY_UP(lua_State* a)
{
	lua_pushnumber(a, KEY_UP);

	return 1;
}

static int bllua_func_KEY_DOWN(lua_State* a)
{
	lua_pushnumber(a, KEY_DOWN);

	return 1;
}

static int bllua_func_KEY_INS(lua_State* a)
{
	lua_pushnumber(a, KEY_INS);

	return 1;
}

static int bllua_func_KEY_DEL(lua_State* a)
{
	lua_pushnumber(a, KEY_DEL);

	return 1;
}

static int bllua_func_BL_PSET(lua_State* a)
{
	lua_pushnumber(a, BL_PSET);

	return 1;
}

static int bllua_func_BL_PAND(lua_State* a)
{
	lua_pushnumber(a, BL_PAND);

	return 1;
}

static int bllua_func_BL_POR(lua_State* a)
{
	lua_pushnumber(a, BL_POR);

	return 1;
}

static int bllua_func_BL_PXOR(lua_State* a)
{
	lua_pushnumber(a, BL_PXOR);

	return 1;
}

static int bllua_func_BL_FULLHEIGHT(lua_State* a)
{
	lua_pushnumber(a, BL_FULLHEIGHT);

	return 1;
}

static int bllua_func_BL_HALFHEIGHT(lua_State* a)
{
	lua_pushnumber(a, BL_HALFHEIGHT);

	return 1;
}

static int bllua_func_BL_DBGFLSH(lua_State* a)
{
	lua_pushnumber(a, BL_DBGFLSH);

	return 1;
}

static int bllua_func_BL_RLSFLSH(lua_State* a)
{
	lua_pushnumber(a, BL_RLSFLSH);

	return 1;
}

static int bllua_func_BL_DEBUG(lua_State* a)
{
	lua_pushnumber(a, BL_DEBUG);

	return 1;
}

static int bllua_func_BL_RELEASE(lua_State* a)
{
	lua_pushnumber(a, BL_RELEASE);

	return 1;
}

static int bllua_func_BL_WAITKEYF(lua_State* a)
{
	lua_pushnumber(a, BL_WAITKEYF);

	return 1;
}

static int bllua_func_BL_WAITKEYNF(lua_State* a)
{
	lua_pushnumber(a, BL_WAITKEYNF);

	return 1;
}

static int bllua_func_BL_WAITKEY(lua_State* a)
{
	lua_pushnumber(a, BL_WAITKEY);

	return 1;
}

static int bllua_func_BL_GETKEY(lua_State* a)
{
	lua_pushnumber(a, BL_GETKEY);

	return 1;
}

static int bllua_func_BL_CLEARREP(lua_State* a)
{
	lua_pushnumber(a, BL_CLEARREP);

	return 1;
}

static int bllua_func_BL_DELFFF(lua_State* a)
{
	lua_pushnumber(a, BL_DELFFF);

	return 1;
}

static int bllua_func_BL_KEYMODE(lua_State* a)
{
	lua_pushnumber(a, BL_KEYMODE);

	return 1;
}

void bllua_regist_bl_func(lua_State* a)
{
	lua_register(a, "bl_openWin", bllua_func_bl_openWin);
	lua_register(a, "bl_putc", bllua_func_bl_putc);
	lua_register(a, "bl_puts", bllua_func_bl_puts);
	lua_register(a, "bl_puts1", bllua_func_bl_puts1);
	lua_register(a, "bl_setCol", bllua_func_bl_setCol);
	lua_register(a, "bl_setBCol", bllua_func_bl_setBCol);
	lua_register(a, "bl_rgb", bllua_func_bl_rgb);
	lua_register(a, "bl_iCol", bllua_func_bl_iCol);
	lua_register(a, "bl_flshWin", bllua_func_bl_flshWin);
	lua_register(a, "bl_getGrpB", bllua_func_bl_getGrpB);
	lua_register(a, "bl_setPix", bllua_func_bl_setPix);
	lua_register(a, "bl_fillRect", bllua_func_bl_fillRect);
	lua_register(a, "bl_drawRect", bllua_func_bl_drawRect);
	lua_register(a, "bl_drawLine", bllua_func_bl_drawLine);
	lua_register(a, "bl_rnd", bllua_func_bl_rnd);
	lua_register(a, "bl_wait", bllua_func_bl_wait);
	lua_register(a, "bl_color", bllua_func_bl_color);
	lua_register(a, "bl_locate", bllua_func_bl_locate);
	lua_register(a, "bl_printf", bllua_func_bl_printf);
	lua_register(a, "bl_getPix", bllua_func_bl_getPix);
	lua_register(a, "bl_waitNF", bllua_func_bl_waitNF);
	lua_register(a, "bl_inkey1", bllua_func_bl_inkey1);
	lua_register(a, "bl_cls", bllua_func_bl_cls);
	lua_register(a, "bl_clock", bllua_func_bl_clock);
	lua_register(a, "bl_inputInt", bllua_func_bl_inputInt);
	lua_register(a, "bl_inputFloat", bllua_func_bl_inputFloat);
	lua_register(a, "bl_scanf", bllua_func_bl_scanf);
	lua_register(a, "bl_vscanf", bllua_func_bl_vscanf);
	lua_register(a, "bl_setMode", bllua_func_bl_setMode);
	lua_register(a, "bl_rand", bllua_func_bl_rand);
	lua_register(a, "bl_srand", bllua_func_bl_srand);
	lua_register(a, "bl_inkey", bllua_func_bl_inkey);
	lua_register(a, "bl_gets", bllua_func_bl_gets);
	lua_register(a, "bl_fillOval", bllua_func_bl_fillOval);
	lua_register(a, "bl_drawStr", bllua_func_bl_drawStr);
	lua_register(a, "bl_openWind", bllua_func_bl_openVWin);
	lua_register(a, "bl_slctWin", bllua_func_bl_slctWin);
	lua_register(a, "bl_copyRct0", bllua_func_bl_copyRct0);
	lua_register(a, "bl_copyRct1", bllua_func_bl_copyRct1);
	lua_register(a, "bl_drawPtrn_r", bllua_func_bl_drawPtrn_r);
	lua_register(a, "bl_drawPtrn_d", bllua_func_bl_drawPtrn_d);
	lua_register(a, "bl_readyWin", bllua_func_bl_readyWin);
	lua_register(a, "bl_setPtrn0", bllua_func_bl_setPtrn0);
	lua_register(a, "bl_drawPtrn_err_r", bllua_func_bl_drawPtrn_err_r);
	lua_register(a, "bl_drawPtrn_err_d", bllua_func_bl_drawPtrn_err_d);

	lua_register(a, "KEY_ENTER", bllua_func_KEY_ENTER);
	lua_register(a, "KEY_ESC", bllua_func_KEY_ESC);
	lua_register(a, "KEY_BACKSPACE", bllua_func_KEY_BACKSPACE);
	lua_register(a, "KEY_TAB", bllua_func_KEY_TAB);
	lua_register(a, "KEY_LEFT", bllua_func_KEY_LEFT);
	lua_register(a, "KEY_RIGHT", bllua_func_KEY_RIGHT);
	lua_register(a, "KEY_UP", bllua_func_KEY_UP);
	lua_register(a, "KEY_DOWN", bllua_func_KEY_DOWN);
	lua_register(a, "KEY_INS", bllua_func_KEY_INS);
	lua_register(a, "KEY_DEL", bllua_func_KEY_DEL);

	lua_register(a, "BL_PSET", bllua_func_BL_PSET);
	lua_register(a, "BL_PAND", bllua_func_BL_PAND);
	lua_register(a, "BLPOR", bllua_func_BL_POR);
	lua_register(a, "BL_PXOR", bllua_func_BL_PXOR);
	lua_register(a, "BL_FULLHEIGHT", bllua_func_BL_FULLHEIGHT);
	lua_register(a, "BL_HALFHEIGHT", bllua_func_BL_HALFHEIGHT);
	lua_register(a, "BL_DBGFLSH", bllua_func_BL_DBGFLSH);
	lua_register(a, "BL_RLSFLSH", bllua_func_BL_RLSFLSH);
	lua_register(a, "BL_DEBUG", bllua_func_BL_DEBUG);
	lua_register(a, "BL_RELEASE", bllua_func_BL_RELEASE);
	lua_register(a, "BL_WAITKEYF", bllua_func_BL_WAITKEYF);
	lua_register(a, "BL_WAITKEYNF", bllua_func_BL_WAITKEYNF);
	lua_register(a, "BL_WAITKEY", bllua_func_BL_WAITKEY);
	lua_register(a, "BL_GETKEY", bllua_func_BL_GETKEY);
	lua_register(a, "BL_CLEARREP", bllua_func_BL_CLEARREP);
	lua_register(a, "BL_DELFFF", bllua_func_BL_DELFFF);
	lua_register(a, "BL_KEYMODE", bllua_func_BL_KEYMODE);
}
