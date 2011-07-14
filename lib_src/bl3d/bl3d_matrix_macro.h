#include "bl3d_math_macro.h"

#ifndef __BL3D_MATRIX_MACRO_H__
#define __BL3D_MATRIX_MACRO_H__

// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define __xxx__(xxx) {			\
}
// SSE3 を使用不可能な場合
#else
#define __xxx__(xxx) {			\
}
#endif // __ENABLE_SSE3__



/// 0ベクトルを代入
///
/// dst: struct BL3D_VECTOR*
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_SET_0_VECTOR(dst) {			\
	__asm__ volatile(				\
		"movaps (%1),   %%xmm0;"		\
		"movaps %%xmm0, (%0);"			\
		:					\
		:"r"((dst)), "r"(&bl3d_0_vector)	\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_SET_0_VECTOR(dst) {			\
	*(dst) = bl3d_0_vector;				\
}
#endif // __ENABLE_SSE3__



/// ベクトル同士の加算
///
/// dst, src: struct BL3D_VECTOR*
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_ADD_VECTOR(dst, src) {			\
	__asm__ volatile(				\
		"movaps (%1),   %%xmm0;"		\
		"addps  (%0),   %%xmm0;"		\
		"movaps %%xmm0, (%1);"			\
		:					\
		:"r"((src)), "r"((dst))			\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_ADD_VECTOR(dst, src) {			\
	(dst)->x += (src)->x;				\
	(dst)->y += (src)->y;				\
	(dst)->z += (src)->z;				\
}
#endif // __ENABLE_SSE3__



/// ベクトル同士の減算
///
/// dst, src: struct BL3D_VECTOR*
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_SUB_VECTOR(dst, src) {			\
	__asm__ volatile(				\
		"movaps (%1),   %%xmm0;"		\
		"subps  (%0),   %%xmm0;"		\
		"movaps %%xmm0, (%1);"			\
		:					\
		:"r"((src)), "r"((dst))			\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_SUB_VECTOR(dst, src) {			\
	(dst)->x -= (src)->x;				\
	(dst)->y -= (src)->y;				\
	(dst)->z -= (src)->z;				\
}
#endif // __ENABLE_SSE3__



/// ベクトル同士の乗算
///
/// dst, src: struct BL3D_VECTOR*
///
/// dst *= src
///
/// 備考：
/// スカラー倍したい場合も、ベクトルにして乗算する必要がある
/// そのベクトル化したスカラー量を使いまわせるならば、高速化を期待できる
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_MUL_VECTOR(dst, src) {			\
	__asm__ volatile(				\
		"movaps (%1),   %%xmm0;"		\
		"mulps  (%0),   %%xmm0;"		\
		"movaps %%xmm0, (%1);"			\
		:					\
		:"r"((src)), "r"((dst))			\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_MUL_VECTOR(dst, src) {			\
	(dst)->x *= (src)->x;				\
	(dst)->y *= (src)->y;				\
	(dst)->z *= (src)->z;				\
}
#endif // __ENABLE_SSE3__



/// ベクトル同士の乗算（dst = src0 * src1）
///
/// dst, src0, src1: struct BL3D_VECTOR*
///
/// dst = src0 * src1
///
/// 備考：
/// スカラー倍したい場合も、ベクトルにして乗算する必要がある
/// そのベクトル化したスカラー量を使いまわせるならば、高速化を期待できる
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_MUL2_VECTOR(dst, src0, src1) {		\
	__asm__ volatile(				\
		"movaps (%0),   %%xmm0;"		\
		"mulps  (%1),   %%xmm0;"		\
		"movaps %%xmm0, (%2);"			\
		:					\
		:"r"((src0)), "r"((src1)), "r"((dst))	\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_MUL2_VECTOR(dst, src0, src1) {		\
	(dst)->x = (src0)->x * (src1)->x;		\
	(dst)->y = (src0)->y * (src1)->y;		\
	(dst)->z = (src0)->z * (src1)->z;		\
}
#endif // __ENABLE_SSE3__



/// ベクトルの差を得る
///
/// dst, esrc, ssrc: struct BL3D_VECTOR*
///
/// esrc - ssrc = dst
/// esrc が終点、 ssrc が始点 
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_DIFF_VECTOR(dst, esrc, ssrc) {		\
	__asm__ volatile(				\
		"movaps (%1),   %%xmm0;"		\
		"subps  (%0),   %%xmm0;"		\
		"movaps %%xmm0, (%2);"			\
		:					\
		:"r"((ssrc)), "r"((esrc)), "r"((dst))	\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_DIFF_VECTOR(dst, esrc, ssrc) {		\
	(dst)->x = (esrc)->x - (ssrc)->x;		\
	(dst)->y = (esrc)->y - (ssrc)->y;		\
	(dst)->z = (esrc)->z - (ssrc)->z;		\
}
#endif // __ENABLE_SSE3__



/// ベクトルの各要素同士を乗算したものを合計した値を得る
/// dst: float*		// __attribute__aligned(16)
/// src0, src1: struct BL3D_VECTOR*
///
/// (Ax, Ay, Az) *= (Bx, By, Bz)
/// Ax + Ay + Az
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_MULADD_VECTOR(dst, src0, src1) {		\
	__asm__ volatile(				\
		"movaps (%0),   %%xmm0;"		\
		"movaps (%1),   %%xmm1;"		\
							\
		"mulps  %%xmm0, %%xmm1;"		\
		"andps  (%3),   %%xmm1;"		\
							\
		"haddps %%xmm1, %%xmm1;"		\
		"haddps %%xmm1, %%xmm1;"		\
		"movss %%xmm1, (%2);"			\
		:					\
		:"r"((src0)), "r"((src1)), "r"((dst)), "r"(bl3d_mask_vector_xyz)	\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_MULADD_VECTOR(dst, src0, src1) {		\
	*(dst) = (src0)->x * (src1)->x + (src0)->y * (src1)->y + (src0)->z * (src1)->z;	\
}
#endif // __ENABLE_SSE3__



/// 転値行列を得る
///
/// dst, src: struct BL3D_MATRIX*
///
/// A B C    A D G
/// D E F -> B E H
/// G H I    C F I
///
/// （注意：平行移動t[]は計算しません）
#define BL3D_TRANSPOSE_MATRIX(dst, src) {			\
	(dst)->m[0][0] = (src)->m[0][0];	(dst)->m[0][1] = (src)->m[1][0];	(dst)->m[0][2] = (src)->m[2][0];	\
	(dst)->m[1][0] = (src)->m[0][1];	(dst)->m[1][1] = (src)->m[1][1];	(dst)->m[1][2] = (src)->m[2][1];	\
	(dst)->m[2][0] = (src)->m[0][2];	(dst)->m[2][1] = (src)->m[1][2];	(dst)->m[2][2] = (src)->m[2][2];	\
}



/// ２つの行列の積をとります。
///
/// dst, src0, src1: struct BL3D_MATRIX*
///
/// 計算結果はdstに格納されます。
/// 計算順序は src0 * src1 = dst です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
/// 
/// （注意： src0 または src1 が dst と同じアドレスの場合、不正な計算結果となります！！）
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_MUL_MATRIX(dst, src0, src1) {		\
	struct BL3D_VECTOR t_src1[3] = {		\
		{.x = (src1)->m[0][0], .y = (src1)->m[1][0], .z = (src1)->m[2][0], .pad = 0},	\
		{.x = (src1)->m[0][1], .y = (src1)->m[1][1], .z = (src1)->m[2][1], .pad = 0},	\
		{.x = (src1)->m[0][2], .y = (src1)->m[1][2], .z = (src1)->m[2][2], .pad = 0}	\
	};						\
							\
	int j;						\
	for(j = 0; j < 3; j++) {			\
		__asm__ volatile(			\
			"movaps (%0),   %%xmm0;"	\
			"andps  (%3),   %%xmm0;"	\
			"movaps 0(%1),  %%xmm1;"	\
							\
			"mulps  %%xmm0, %%xmm1;"	\
							\
			"haddps %%xmm1, %%xmm1;"	\
			"haddps %%xmm1, %%xmm1;"	\
			"movss  %%xmm1, 0(%2);"		\
							\
							\
			"movaps 16(%1), %%xmm1;"	\
							\
			"mulps  %%xmm0, %%xmm1;"	\
							\
			"haddps %%xmm1, %%xmm1;"	\
			"haddps %%xmm1, %%xmm1;"	\
			"movss  %%xmm1, 4(%2);"		\
							\
							\
			"movaps 32(%1), %%xmm1;"	\
							\
			"mulps  %%xmm0, %%xmm1;"	\
							\
			"haddps %%xmm1, %%xmm1;"	\
			"haddps %%xmm1, %%xmm1;"	\
			"movss  %%xmm1, 8(%2);"		\
			:				\
			:"r"((src0)->m[j]), "r"(t_src1), "r"((dst)->m[j]), "r"(bl3d_mask_vector_xyz)	\
			:"memory"			\
		);					\
	}						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_MUL_MATRIX(dst, src0, src1) {		\
	int j, i;					\
							\
	for(j=0; j<3; j++) {				\
		for(i=0; i<3; i++) {			\
			(dst)->m[j][i] =			\
				(src0)->m[j][0] * (src1)->m[0][i] +		\
				(src0)->m[j][1] * (src1)->m[1][i] +		\
				(src0)->m[j][2] * (src1)->m[2][i];		\
		}					\
	}						\
}
#endif // __ENABLE_SSE3__



/// ベクトルに行列を乗算します。
///
/// dst, vsrc: struct BL3D_VECTOR*
/// msrc: struct BL3D_MATRIX*
///
/// 計算結果はdstに格納されます。
/// 計算順序は m * v = dst です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_APPLY_MATRIX(dst, msrc, vsrc) {		\
	__asm__ volatile(				\
		"movaps 0(%0),  %%xmm0;"		\
		"movaps (%1),   %%xmm1;"		\
							\
		"mulps  %%xmm1, %%xmm0;"		\
		"andps  (%3),   %%xmm0;"		\
							\
		"haddps %%xmm0, %%xmm0;"		\
		"haddps %%xmm0, %%xmm0;"		\
		"movss  %%xmm0, 0(%2);"			\
							\
							\
		"movaps 16(%0), %%xmm0;"		\
							\
		"mulps  %%xmm1, %%xmm0;"		\
		"andps  (%3),   %%xmm0;"		\
							\
		"haddps %%xmm0, %%xmm0;"		\
		"haddps %%xmm0, %%xmm0;"		\
		"movss  %%xmm0, 4(%2);"			\
							\
							\
		"movaps 32(%0), %%xmm0;"		\
							\
		"mulps  %%xmm1, %%xmm0;"		\
		"andps  (%3),   %%xmm0;"		\
							\
		"haddps %%xmm0, %%xmm0;"		\
		"haddps %%xmm0, %%xmm0;"		\
		"movss  %%xmm0, 8(%2);"			\
		:					\
		:"r"((msrc)->m[0]), "r"((vsrc)), "r"((dst)), "r"(bl3d_mask_vector_xyz)	\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_APPLY_MATRIX(dst, msrc, vsrc) {		\
	struct BL3D_VECTOR tmp_vsrc = *(vsrc);		\
	float* ptr_dst = &((dst)->x);			\
							\
	int j;						\
							\
	for(j=0; j<3; j++) {				\
		ptr_dst[j] =				\
			(msrc)->m[j][0] * tmp_vsrc.x + 	\
			(msrc)->m[j][1] * tmp_vsrc.y + 	\
			(msrc)->m[j][2] * tmp_vsrc.z;	\
	}						\
}
#endif // __ENABLE_SSE3__



/// x軸の回転角から回転行列を求める。
/// dst: struct BL3D_MATRIX*
/// src: float			// 非ポインター
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
#define BL3D_ROT_MATRIX_X(dst, src) {			\
	const float s = BL3D_SIN((src));		\
	const float c = BL3D_COS((src));		\
							\
	(dst)->m[0][0] = 1;	(dst)->m[0][1] = 0;	(dst)->m[0][2] = 0;	\
	(dst)->m[1][0] = 0;	(dst)->m[1][1] = c;	(dst)->m[1][2] = -s;	\
	(dst)->m[2][0] = 0;	(dst)->m[2][1] = s;	(dst)->m[2][2] = c;	\
}



/// y軸の回転角から回転行列を求める。
/// dst: struct BL3D_MATRIX*
/// src: float			// 非ポインター
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
#define BL3D_ROT_MATRIX_Y(dst, src) {			\
	const float s = BL3D_SIN((src));		\
	const float c = BL3D_COS((src));		\
							\
	(dst)->m[0][0] = c;	(dst)->m[0][1] = 0;	(dst)->m[0][2] = s;	\
	(dst)->m[1][0] = 0;	(dst)->m[1][1] = 1;	(dst)->m[1][2] = 0;	\
	(dst)->m[2][0] = -s;	(dst)->m[2][1] = 0;	(dst)->m[2][2] = c;	\
}



/// z軸の回転角から回転行列を求める。
/// dst: struct BL3D_MATRIX*
/// src: float			// 非ポインター
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
#define BL3D_ROT_MATRIX_Z(dst, src) {			\
	const float s = BL3D_SIN((src));		\
	const float c = BL3D_COS((src));		\
							\
	(dst)->m[0][0] = c;	(dst)->m[0][1] = -s;	(dst)->m[0][2] = 0;	\
	(dst)->m[1][0] = s;	(dst)->m[1][1] = c;	(dst)->m[1][2] = 0;	\
	(dst)->m[2][0] = 0;	(dst)->m[2][1] = 0;	(dst)->m[2][2] = 1;	\
}



/// 回転角から回転行列を求める。
///
/// dst: struct BL3D_MATRIX*
/// src: struct BL3D_VECTOR*
///
/// 回転行列の乗算順序は z * y * x = m です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
#define BL3D_ROT_MATRIX(dst, src) {			\
	struct BL3D_MATRIX mx, my, mz, tmp;		\
							\
	BL3D_ROT_MATRIX_X(&mx, (src)->x);		\
	BL3D_ROT_MATRIX_Y(&my, (src)->y);		\
	BL3D_ROT_MATRIX_Z(&mz, (src)->z);		\
							\
	BL3D_MUL_MATRIX(&tmp, &mz,  &my);		\
	BL3D_MUL_MATRIX((dst),  &tmp, &mx);		\
}



/// 平行移動量を行列にセットする。
///
/// dst: struct BL3D_MATRIX*
/// src: struct BL3D_VECTOR*
///
/// 計算結果は m に格納されます。
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_TRANS_MATRIX(dst, src) {			\
	__asm__ volatile(				\
		"movaps (%1),   %%xmm0;"		\
		"movaps %%xmm0, (%0);"			\
		:					\
		:"r"((dst)->t), "r"((src))		\
		:"memory"				\
	);						\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_TRANS_MATRIX(dst, src) {			\
	float* p = &((src)->x);				\
							\
	(dst)->t[0] = p[0];				\
	(dst)->t[1] = p[1];				\
	(dst)->t[2] = p[2];				\
}
#endif // __ENABLE_SSE3__



/// 座標変換の合成を行います。
///
/// dst, src0, src1: struct BL3D_MATRIX*
///
/// bl3d_mul_matrix()とは異なり、平行移動も含めて合成します。
/// 計算結果はdstに格納されます。
///
/// 行列の計算順序は src0.m * src1.m = dst.m です。
/// 平行移動の計算順序は (src0.m * src1.t) + src0.t = dst.t です。
/// つまり src0 が親側で、src1が子側です。
#define BL3D_COMP_MATRIX(dst, src0, src1) {			\
	struct BL3D_MATRIX tmp;					\
								\
	BL3D_MUL_MATRIX(&tmp, (src0), (src1));			\
								\
	BL3D_APPLY_MATRIX(					\
		(struct BL3D_VECTOR*)(tmp.t),			\
		src0,						\
		(struct BL3D_VECTOR*)(src1->t)			\
	);							\
								\
	tmp.t[0] += (src0)->t[0];				\
	tmp.t[1] += (src0)->t[1];				\
	tmp.t[2] += (src0)->t[2];				\
								\
	*(dst) = tmp;						\
}



/// ベクトルのノルムを返す
///
/// dst: float*
/// src: struct BL3D_VECTOR*
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_NORM_VECTOR(dst, src) {				\
	__asm__ volatile(					\
		"movaps (%0),   %%xmm0;"			\
		"andps  (%2),   %%xmm0;"			\
								\
		"mulps  %%xmm0, %%xmm0;"			\
		"haddps %%xmm0, %%xmm0;"			\
		"haddps %%xmm0, %%xmm0;"			\
		"sqrtss %%xmm0, %%xmm0;"			\
								\
		"movss  %%xmm0, (%1);"				\
		:						\
		:"r"((src)), "r"((dst)), "r"(bl3d_mask_vector_xyz)	\
		:"memory"					\
	);							\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_NORM_VECTOR(dst, src) {				\
	*(dst) = ((src)->x * (src)->x) + ((src)->y * (src)->y) + ((src)->z * (src)->z);		\
	*(dst) = BL3D_SQRT(*(dst));				\
}
#endif // __ENABLE_SSE3__



///ベクトルのノルムの逆数を返す
///
/// dst: float*
/// src: struct BL3D_VECTOR*
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_INVERT_NORM_VECTOR(dst, src) {			\
	__asm__ volatile(					\
		"movaps  (%0),   %%xmm0;"			\
								\
		"mulps   %%xmm0, %%xmm0;"			\
		"andps   (%2),   %%xmm0;"			\
								\
		"haddps  %%xmm0, %%xmm0;"			\
		"haddps  %%xmm0, %%xmm0;"			\
		"rsqrtss %%xmm0, %%xmm0;"			\
								\
		"movss   %%xmm0, (%1);"				\
		:						\
		:"r"((src)), "r"((dst)), "r"(bl3d_mask_vector_xyz)	\
		:"memory"					\
	);							\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_INVERT_NORM_VECTOR(dst, src) {			\
	*(dst) = ((src)->x * (src)->x) + ((src)->y * (src)->y) + ((src)->z * (src)->z);		\
	*(dst) = BL3D_SQRT(*(dst));				\
	*(dst) = (*dst != 0)? (1.0 / *(dst)): 0;		\
}
#endif // __ENABLE_SSE3__



/// ベクトルの正規化
///
/// dst, src, struct BL3D_VECTOR*
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_UNIT_VECTOR(dst, src) {				\
	const float __attribute__((aligned(16)))tmp;		\
								\
	BL3D_INVERT_NORM_VECTOR(&tmp, (src));			\
								\
	float __attribute__((aligned(16)))invert_norm[4] = {tmp, tmp, tmp, 0};	\
								\
	__asm__ volatile(					\
		"movaps  (%0),   %%xmm0;"			\
		"mulps   (%2),   %%xmm0;"			\
		"movaps  %%xmm0, (%1);"				\
		:						\
		:"r"((src)), "r"((dst)), "r"(invert_norm)	\
		:"memory"					\
	);							\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_UNIT_VECTOR(dst, src) {				\
	float __attribute__((aligned(16)))invert_norm;		\
								\
	BL3D_INVERT_NORM_VECTOR(&invert_norm, (src));		\
								\
	(dst)->x = (src)->x * invert_norm;			\
	(dst)->y = (src)->y * invert_norm;			\
	(dst)->z = (src)->z * invert_norm;			\
}
#endif // __ENABLE_SSE3__



/// ２ベクトルから外積を得る
///
/// dst, src0, src1: struct BL3D_VECTOR*
///
// SSE3 を使用可能な場合
#define BL3D_OUTER_PRODUCT_VECTOR(dst, src0, src1) {		\
	struct BL3D_MATRIX m_src1 = {				\
		.m[0][0]= 0,		.m[0][1]= (src1)->z,	.m[0][2]=-(src1)->y,	\
		.m[1][0]=-(src1)->z,	.m[1][1]= 0,		.m[1][2]= (src1)->x,	\
		.m[2][0]= (src1)->y,	.m[2][1]=-(src1)->x,	.m[2][2]= 0		\
	};							\
								\
	BL3D_APPLY_MATRIX((dst), &m_src1, (src0));		\
								\
	BL3D_UNIT_VECTOR((dst), (dst));				\
}



/// ベクトル同士の内積を得る
///
/// dst: float*
/// src0, src1: struct BL3D_VECTOR*
///
#define BL3D_INNER_PRODUCT_VECTOR(dst, src0, src1) {		\
	BL3D_MULADD_VECTOR((dst), (src0), (src1));		\
								\
	if(*(dst) < 0) {					\
		*(dst) = 0;					\
	}							\
}



/// 三角形の頂点ベクトルから法線ベクトルを得る。
///
/// dst, vertex0, vertex1, vertex2: struct BL3D_VECTOR*
///
#define BL3D_GET_NORMAL_TRIANGLE(dst, vertex0, vertex1, vertex2) {	\
	struct BL3D_VECTOR ___A___;					\
	BL3D_DIFF_VECTOR(&___A___, (vertex1), (vertex0));		\
									\
	struct BL3D_VECTOR ___B___;					\
	BL3D_DIFF_VECTOR(&___B___, (vertex2), (vertex1));		\
									\
	BL3D_OUTER_PRODUCT_VECTOR((dst), &___A___, &___B___);		\
}



/// struct BL3D_CVECTOR を 0x00RRGGBB形式の 32bit color へ変換
///
/// dst: int[4]の先頭アドレス。結果は[0]に格納される。（16byte境界であること）
/// src: struct BL3D_CVECTOR*
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_CVECTOR_TO_INTCOLOR(dst, src) {				\
	__asm__ volatile(						\
		"movaps  (%0),    %%xmm0;"				\
		"mulps   (%2),    %%xmm0;"				\
									\
		"cvtps2dq %%xmm0, %%xmm1;"				\
									\
		"packssdw %%xmm0, %%xmm1;"				\
		"packuswb %%xmm0, %%xmm1;"				\
									\
		"movd     %%xmm1, (%1);"				\
		:							\
		:"r"((src)), "r"((dst)), "r"(&bl3d_mul255_vector)	\
		:"memory"						\
	);								\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_CVECTOR_TO_INTCOLOR(dst, src) {				\
		BL3D_MUL_VECTOR((struct BL3D_VECTOR*)(src), &bl3d_mul255_vector);	\
									\
		if((src)->r > 255) {(src)->r = 255;}			\
		if((src)->g > 255) {(src)->g = 255;}			\
		if((src)->b > 255) {(src)->b = 255;}			\
									\
		*(dst) =						\
			(((int)(src)->r & 0xFF) << 16) |		\
			(((int)(src)->g & 0xFF) << 8)  |		\
			(((int)(src)->b & 0xFF));			\
}
#endif // __ENABLE_SSE3__



/// キャッシュへデータを先読みする
///
/// src: 先読みするメモリーアドレス
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_PREFETCH(src) {					\
	__asm__ volatile (					\
		"prefetchnta (%0);"				\
		:						\
		:"r"((src))					\
	);							\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_PREFETCH(src) {					\
}
#endif // __ENABLE_SSE3__



#endif // __BL3D_MATRIX_MACRO_H__
