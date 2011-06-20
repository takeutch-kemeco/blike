#include <glib.h>
#include <gdk/gdkkeysyms.h>
#include "drvgtk_key_ring_buffer.h"
#include "blike0.h"

gint32 transrate_keycode_DrvGtkKey(struct DrvGtkKey* a)
{
	// 入力が無いときは０を返す
	if(a->state == GDK_KEY_VoidSymbol) {
		return 0;
	}
	
	
	gint32 k;
	
	// GtkのキーコードをBlikeのキーコードへ変換
	// 該当しないキーコードはそのまま返す
	switch(a->value) {
	case GDK_KEY_Return:		k = KEY_ENTER;
	case GDK_KEY_Escape:		k = KEY_ESC;
	case GDK_KEY_BackSpace:		k = KEY_BACKSPACE;
	case GDK_KEY_Tab:		k = KEY_TAB;
	case GDK_KEY_Left:		k = KEY_LEFT;
	case GDK_KEY_Right:		k = KEY_RIGHT;
	case GDK_KEY_Up:		k = KEY_UP;
	case GDK_KEY_Down:		k = KEY_DOWN;
	case GDK_KEY_Insert:		k = KEY_INS;
	case GDK_KEY_Delete:		k = KEY_DEL;
	default:			k = a->value;
	}
	
	
	// キーを離した瞬間は 4095 を返す
	if(a->state == DrvGtkKeyState_release) {
//		k += 100000000;
		k = 0x0FFF;
	}
	
	
	return k;
}
