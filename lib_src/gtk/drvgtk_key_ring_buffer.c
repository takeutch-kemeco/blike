#include <glib.h>
#include "drvgtk_key_ring_buffer.h"

struct DrvGtkKeyRingBuffer* new_DrvGtkKeyRingBuffer(guint size)
{
	struct DrvGtkKeyRingBuffer* a = g_malloc(sizeof(*a));
	
	a->key = g_malloc(sizeof(*(a->key)) * size);
	a->key_len = size;
	a->read_index = 0;
	a->write_index = 0;
	
	return a;
}

void write_c_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a, struct DrvGtkKey* key)
{
	guint32 index = a->write_index;
	
	index++;
	if(index >= a->key_len) {
		index = 0;
	}
	
	a->key[index] = *key;
	a->write_index = index;
}

void read_c_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a, struct DrvGtkKey* key)
{
	if(a->read_index == a->write_index) {
		key->state = DrvGtkKeyState_none;
		key->value = 0;
	}
	else {
		guint32 index = a->read_index;
	
		index++;
		if(index >= a->key_len) {
			index = 0;
		}
		
		*key = a->key[index];
		a->read_index = index;
	}
}

void read_c_view_only_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a, struct DrvGtkKey* key)
{
	if(a->read_index == a->write_index) {
		key->state = DrvGtkKeyState_none;
		key->value = 0;
	}
	else {
		guint32 index = a->read_index;
	
		index++;
		if(index >= a->key_len) {
			index = 0;
		}
		
		*key = a->key[index];
	}
}

void clean_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a)
{
	a->read_index = a->write_index;
}



