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

void bl3d_copy_blike_to_frame_buffer(
	struct BL3D_FRAME_BUFFER* fb,
	int vram_number,
	int vram_width,
	int vram_height
)
{
	bl_slctWin(vram_number);
	
	const float scale = 1.0/255;
	
	int j, i;
	for(j = 0; j < vram_height; j++) {
		for(i = 0; i < vram_width; i++) {
			int C = bl_getPix(j, i);
			
			(fb->y_offset_table[j] + i)->r = (C >> 16);
			(fb->y_offset_table[j] + i)->g = (C >> 8 );
			(fb->y_offset_table[j] + i)->b = (C >> 0 );

			(fb->y_offset_table[j] + i)->r *= scale;
			(fb->y_offset_table[j] + i)->g *= scale;
			(fb->y_offset_table[j] + i)->b *= scale;

		}
	}
}

void bl3d_copy_frame_buffer_to_blike(struct BL3D_FRAME_BUFFER* fb)
{
	const float scale = 255.0;
	
	int* p = fb->copy_buffer;
	
	int j, i;
	for(j = 0; j < fb->draw_height; j++) {
		for(i = 0; i < fb->draw_width; i++) {
			int col_r = (fb->y_offset_table[j] + i)->r * 255;
			int col_g = (fb->y_offset_table[j] + i)->g * 255;
			int col_b = (fb->y_offset_table[j] + i)->b * 255;
			
			*p++ = (col_r << 16) | (col_g << 8) | (col_b);
		}
	}
	
	bl_slctWin(0);

}

