#include <glib.h>

#ifndef __DRVGTK_KEY_RING_BUFFER_H__
#define __DRVGTK_KEY_RING_BUFFER_H__

enum DrvGtkKeyState {
	DrvGtkKeyState_press	= 1,
	DrvGtkKeyState_release	= 2,
	DrvGtkKeyState_none	= 0,
};

struct DrvGtkKey {
	guint32			value;
	enum DrvGtkKeyState	state;
};

struct DrvGtkKeyRingBuffer {
	struct DrvGtkKey*	key;
	guint32			key_len;
	
	guint32			read_index;
	guint32			write_index;
};

extern struct DrvGtkKeyRingBuffer* new_DrvGtkKeyRingBuffer(guint size);

extern void write_c_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a, struct DrvGtkKey* key);

extern void read_c_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a, struct DrvGtkKey* key);
extern void read_c_view_only_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a, struct DrvGtkKey* key);

extern void clean_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer* a);

#endif // __DRVGTK_KEY_RING_BUFFER_H__
