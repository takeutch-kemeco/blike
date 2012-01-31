#include <bl3d_types.h>

#ifndef __BL3D_MATH_H__
#define __BL3D_MATH_H__

/// 面の方程式
struct BL3D_SURFACE_EQUATION{
	float a, b, c, d;
} __attribute((aligned(16)));

extern inline float bl3d_rcp(const float a);
extern inline float bl3d_sqrt(const float a);
extern inline float bl3d_atan2(const float a, const float b);

extern struct BL3D_SURFACE_EQUATION* bl3d_triangle_to_surface_equation(
	struct BL3D_SURFACE_EQUATION* a,
	struct BL3D_VECTOR* vertex0,
	struct BL3D_VECTOR* vertex1,
	struct BL3D_VECTOR* vertex2
);

#endif //__BL3D_MATH_H__
