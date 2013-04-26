#include <glib.h>

#ifndef __DRVGTK_KEY_RING_BUFFER_H__
#define __DRVGTK_KEY_RING_BUFFER_H__

enum DrvGtkKeyState {
        DrvGtkKeyState_press   = 1,
        DrvGtkKeyState_release = 2,
        DrvGtkKeyState_none    = 0,
};

struct DrvGtkKey {
        guint32 value;
        enum DrvGtkKeyState state;
};

struct DrvGtkKeyRingBuffer {
        struct DrvGtkKey *key;
        gint32 key_len;
        gint32 *int_key;
        gint32 *read_index;
        gint32 *write_index;
        gint32 *key_count;
};

struct DrvGtkKeyRingBuffer* new_DrvGtkKeyRingBuffer(gint32 key_len,
                                                    gint32 *int_key,
                                                    gint32 *read_index,
                                                    gint32 *write_index,
                                                    gint32 *key_count);
void free_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer *a);
void write_c_DrvGtkKeyRingBuffer(struct DrvGtkKeyRingBuffer *a,
                                 struct DrvGtkKey *key);

#endif // __DRVGTK_KEY_RING_BUFFER_H__
