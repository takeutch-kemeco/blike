#include <stdlib.h>

#ifndef __DRVLFB_MALLOC_H__
#define __DRVLFB_MALLOC_H__

void* drvlfb_malloc_aligned16(size_t size);
void* drvlfb_malloc_0_aligned16(size_t size);
void drvlfb_free_aligned16(void *a);

#endif // __DRVLFB_MALLOC_H__
