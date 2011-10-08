#ifndef __BL_MALLOC_H__
#define __BL_MALLOC_H__

void* bl_malloc_aligned(unsigned int bytes, unsigned int align);
void bl_free_aligned(void* a);

#endif // __BL_MALLOC_H__

