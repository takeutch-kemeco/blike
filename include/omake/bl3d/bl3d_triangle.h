#include <bl3d_types.h>

#ifndef __BL3D_TRIANGLE_H__
#define __BL3D_TRIANGLE_H__

/// BL3Dで描画できる三角形の、各種タイプ
#define BL3D_TRIANGLE_TYPE_F	(0+0)	/// フラットシェーディング、テクスチャー無し
#define BL3D_TRIANGLE_TYPE_F_T	(0+16)	/// フラットシェーディング、テクスチャー有り
#define BL3D_TRIANGLE_TYPE_G	(8+0)	/// グラデーションシェーディング、テクスチャー無し
#define BL3D_TRIANGLE_TYPE_G_T 	(8+16)	/// グラデーションシェーディング、テクスチャー有り

struct BL3D_TRIANGLE_G_T {
 	int			type;
	int			texture_vram;
	struct BL3D_VECTOR	vertex[3];
	struct BL3D_VECTOR	texture[3];
	struct BL3D_CVECTOR	color[3];
} __attribute__((aligned(16)));

extern void bl3d_init_triangle_g_t(
	struct BL3D_TRIANGLE_G_T*	a,
	struct BL3D_VECTOR*		vertex0,
	struct BL3D_VECTOR*		vertex1,
	struct BL3D_VECTOR*		vertex2,
	struct BL3D_VECTOR*		texture0,
	struct BL3D_VECTOR*		texture1,
	struct BL3D_VECTOR*		texture2,
	int				texture_vram,
	struct BL3D_CVECTOR*		color0,
	struct BL3D_CVECTOR*		color1,
	struct BL3D_CVECTOR*		color2
);



extern void bl3d_sort_triangle_g_t(
	struct BL3D_TRIANGLE_G_T*	a,
	struct BL3D_OT*			ot
);

extern void bl3d_draw_triangle_g_t(struct BL3D_OT_TAG* a);

#endif // __BL3D_TRIANGLE_H__
