#include "bl3d.h"

extern void* bld_malloc(unsigned int bytes);
extern void bld_free(void* p, unsigned int bytes);

struct BL3D_FRAME_BUFFER* bl3d_new_frame_buffer(
	const int width,
	const int height,
	const int offset_x0,
	const int offset_y0,
	const int draw_width,
	const int draw_height
)
{
	struct BL3D_FRAME_BUFFER* a = bld_malloc(sizeof(*a));

	a->pixel = bld_malloc(sizeof(*(a->pixel)) * (width * height));
	a->copy_buffer = bld_malloc(sizeof(*(a->copy_buffer)) * (draw_width * draw_height));

	a->__y_offset_table = bld_malloc(sizeof(*(a->__y_offset_table)) * height);
	a->y_offset_table = a->__y_offset_table + offset_y0;
	
	int j;
	for(j = 0; j < height; j++) {
		a->__y_offset_table[j] = &(a->pixel[(j * width) + offset_x0]);
	}
	
	
	a->frame_buffer_width = width;
	a->frame_buffer_height = height;
	
	a->draw_offset_x0 = offset_x0;
	a->draw_offset_y0 = offset_y0;
	
	a->draw_width = draw_width;
	a->draw_height = draw_height;
	
	
	return a;
}
