#include <glib.h>

#ifndef __DRVGTK_MALLOC_H__
#define __DRVGTK_MALLOC_H__

gpointer drvgtk_malloc_aligned16(gsize size);
gpointer drvgtk_malloc_0_aligned16(gsize size);
void drvgtk_free_aligned16(gpointer a);

#endif // __DRVGTK_MALLOC_H__
