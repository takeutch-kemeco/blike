#include <math.h>
#include "bl3d.h"
#include "bl3d_matrix_macro.h"
#include "bl3d_math_macro.h"

inline float bl3d_rcp(const float a)
{
	float ret;
	BL3D_RCP(&ret, &a);
	return ret;
}

inline float bl3d_sqrt(const float a)
{
	float ret;
	BL3D_SQRT(&ret, &a);
	return ret;
}

inline float bl3d_atan2(const float a, const float b)
{
	return BL3D_ATAN2(a, b);
}

/// ３角形の頂点から面の方程式を得る
struct BL3D_SURFACE_EQUATION* bl3d_triangle_to_surface_equation(
	struct BL3D_SURFACE_EQUATION* a,
	struct BL3D_VECTOR* vertex0,
	struct BL3D_VECTOR* vertex1,
	struct BL3D_VECTOR* vertex2
)
{
	struct BL3D_VECTOR A,B,N;
	BL3D_DIFF_VECTOR(&A, vertex1, vertex0);
	BL3D_DIFF_VECTOR(&B, vertex2, vertex1);
	BL3D_OUTER_PRODUCT_VECTOR(&N, &A, &B);
	
	float d;
	BL3D_INNER_PRODUCT_VECTOR(&d, vertex0, &N);
	
	a->a = N.x;
	a->b = N.y;
	a->c = N.z;
	a->d = -d;
	
	return a;
}
