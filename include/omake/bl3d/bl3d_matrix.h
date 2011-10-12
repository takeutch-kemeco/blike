#include <bl3d_types.h>

#ifndef __BL3D_MATRIX_H__
#define __BL3D_MATRIX_H__

/// float型の x, y, z, pad で padを0にする目的のマスク
/// （padにゴミが入ってると、SSEでのノルム計算の際に正しく計算できないので、padは0でなければならない）
/// これをローカル変数にすると、データの作成時間が勿体無いので。
extern const unsigned long __attribute__((aligned(16)))bl3d_mask_vector_xyz[4];



extern inline struct BL3D_VECTOR* bl3d_add_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src
);

extern inline struct BL3D_VECTOR* bl3d_sub_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src
);

extern inline struct BL3D_VECTOR* bl3d_mul_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src
);

extern inline struct BL3D_VECTOR* bl3d_mul2_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src0,
	struct BL3D_VECTOR* src1
);

extern inline struct BL3D_VECTOR* bl3d_diff_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* esrc,
	struct BL3D_VECTOR* ssrc
);

extern inline float bl3d_muladd_vector(
	struct BL3D_VECTOR* a,
	struct BL3D_VECTOR* b
);

extern inline struct BL3D_MATRIX* bl3d_transpose_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src
);

extern struct BL3D_MATRIX* bl3d_mul_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src0,
	struct BL3D_MATRIX* src1
);

extern inline struct BL3D_VECTOR* bl3d_apply_matrix(
	struct BL3D_VECTOR* dst,
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* v
);

extern inline struct BL3D_MATRIX* bl3d_rot_matrix_x(
	struct BL3D_MATRIX* 	m,
	const float		r
);

extern inline struct BL3D_MATRIX* bl3d_rot_matrix_y(
	struct BL3D_MATRIX* 	m,
	const float		r
);

extern inline struct BL3D_MATRIX* bl3d_rot_matrix_z(
	struct BL3D_MATRIX* 	m,
	const float		r
);

extern inline struct BL3D_MATRIX* bl3d_rot_matrix(
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* r
);

extern inline struct BL3D_MATRIX* bl3d_trans_matrix(
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* v
);

extern struct BL3D_MATRIX* bl3d_comp_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src0,
	struct BL3D_MATRIX* src1
);

extern inline float bl3d_norm_vector(struct BL3D_VECTOR* a);

extern inline float bl3d_invert_norm_vector(struct BL3D_VECTOR* a);

extern inline struct BL3D_VECTOR* bl3d_unit_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src
);

extern inline struct BL3D_VECTOR* bl3d_outer_product_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* A,
	struct BL3D_VECTOR* B
);

extern inline float bl3d_inner_product_vector(
	struct BL3D_VECTOR* A,
	struct BL3D_VECTOR* B
);

extern struct BL3D_MATRIX* bl3d_invert_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src
);

extern inline struct BL3D_VECTOR* bl3d_reflection_vector(
        struct BL3D_VECTOR* dst,
        struct BL3D_VECTOR* view,
        struct BL3D_VECTOR* normal
);

extern void bl3d_print_matrix(struct BL3D_MATRIX* a);

#endif // __BL3D_MATRIX_H__
