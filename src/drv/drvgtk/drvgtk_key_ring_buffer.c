#include <glib.h>
#include "config.h"

#include <gdk/gdkkeysyms.h>
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_transrate_keycode.h"

struct DrvGtkKeyRingBuffer* new_DrvGtkKeyRingBuffer(
	gint32 key_len,
	gint32* int_key,
	gint32* read_index,
	gint32* write_index,
	gint32* key_count
)
{
	struct DrvGtkKeyRingBuffer* a = g_malloc(sizeof(*a));
	
	a->key		= g_malloc(sizeof(*(a->key)) * key_len);
	a->key_len	= key_len;
	
	a->int_key	= int_key;
	a->read_index	= read_index;
	a->write_index	= write_index;
	a->key_count	= key_count;
	
	return a;
}

void free_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a)
{
	g_free(a);
}

extern void bl_putKeyB(int n, int *p);

void write_c_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a, struct DrvGtkKey* key)
{
	gint32 c = transrate_keycode_DrvGtkKey(key);

	switch(key->state) {
	case DrvGtkKeyState_none:
		break;

	case DrvGtkKeyState_press:
		bl_putKeyB(1, &c);
		break;

	case DrvGtkKeyState_release:
		bl_putKeyB(1, &c);
		break;
	}
}
