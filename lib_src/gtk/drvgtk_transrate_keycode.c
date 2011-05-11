#include <glib.h>
#include <gdk/gdkkeysyms.h>
#include "drvgtk_key_ring_buffer.h"
#include "blike0.h"

guint32 transrate_keycode_DrvGtkKey(struct DrvGtkKey* a)
{
	// 入力が無いときは０を返す
	if(a->state == GDK_KEY_VoidSymbol) {
		return 0;
	}

	
	// キーを離した瞬間は 4095 を返す
	if(a->state == DrvGtkKeyState_release) {
		return 0x0FFF;
	}
	
	
	// GtkのキーコードをBlikeのキーコードへ変換
	switch(a->value) {
	case GDK_KEY_Return:		return KEY_ENTER;
	case GDK_KEY_Escape:		return KEY_ESC;
	case GDK_KEY_BackSpace:		return KEY_BACKSPACE;
	case GDK_KEY_Tab:		return KEY_TAB;
	case GDK_KEY_Left:		return KEY_LEFT;
	case GDK_KEY_Right:		return KEY_RIGHT;
	case GDK_KEY_Up:		return KEY_UP;
	case GDK_KEY_Down:		return KEY_DOWN;
	case GDK_KEY_Insert:		return KEY_INS;
	case GDK_KEY_Delete:		return KEY_DEL;
	}
	
	
	// 該当しないキーコードはそのまま返す（LSB側から16bit分）
	return (a->value) & 0xFFFF;
}
