#include <bl3d_types.h>

#ifndef __BL3D_FRAME_BUFFER_H__
#define __BL3D_FRAME_BUFFER_H__

struct BL3D_FRAME_BUFFER {
	int 			frame_buffer_width;
	int 			frame_buffer_height;
	int			draw_offset_x0;
	int			draw_offset_y0;
	int			draw_width;
	int			draw_height;
	struct BL3D_CVECTOR** 	__y_offset_table;
	struct BL3D_CVECTOR** 	y_offset_table;
	struct BL3D_CVECTOR* 	pixel;
	int*			copy_buffer;
} __attribute((aligned(16)));

extern struct BL3D_FRAME_BUFFER* bl3d_new_frame_buffer(
	const int width,
	const int height,
	const int offset_x0,
	const int offset_y0,
	const int draw_width,
	const int draw_height
);

#endif // __BL3D_FRAME_BUFFER_H__
