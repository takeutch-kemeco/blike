#include "blike.h"
#include "bl3d.h"
#include "bl3d_matrix_macro.h"

void print_vector(struct BL3D_VECTOR* a)
{
	printf("[%f, %f, %f, %f]\n", a->x, a->y, a->z, a->pad);
}

extern f();
extern _f();

blMain()
{
	f();
	_f();
}

f()
{
	struct BL3D_VECTOR a = {1,2,3,4};
	struct BL3D_VECTOR b = {5,6,7,8};
	float __attribute__((aligned(16)))c;
	struct BL3D_MATRIX m0;	bl3d_rot_matrix(&m0, &a);
	struct BL3D_MATRIX m1;	bl3d_rot_matrix(&m1, &a);
	struct BL3D_MATRIX m2;	bl3d_rot_matrix(&m0, &a);

	int i;
	for(i=0; i<1000; i++) {
//		BL3D_APPLY_MATRIX(&b, &m0, &a);
	}
}

_f()
{
	struct BL3D_VECTOR a = {1,2,3,4};
	struct BL3D_VECTOR b = {5,6,7,8};
	float __attribute__((aligned(16)))c;
	struct BL3D_MATRIX m0;	bl3d_rot_matrix(&m0, &a);
	struct BL3D_MATRIX m1;	bl3d_rot_matrix(&m1, &a);
	struct BL3D_MATRIX m2;	bl3d_rot_matrix(&m0, &a);
	
	bl3d_print_matrix(&m0);
	bl3d_print_matrix(&m1);

	
	BL3D_APPLY_MATRIX(&b, &m0, &a);
	print_vector(&b);

	bl3d_apply_matrix(&b, &m0, &a);
	print_vector(&b);


	wait(-1);
}
















