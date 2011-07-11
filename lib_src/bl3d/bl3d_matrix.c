//#define __DEBUG__

#include "bl3d.h"
#include "bl3d_matrix_macro.h"

/// float型の x, y, z, pad で padを0にする目的のマスク
/// （padにゴミが入ってると、SSEでのノルム計算の際に正しく計算できないので、padは0でなければならない）
/// これをローカル変数にすると、データの作成時間が勿体無いので。
const unsigned long __attribute__((aligned(16)))bl3d_mask_vector_xyz[4] = {
	0xFFFFFFFF,
	0xFFFFFFFF,
	0xFFFFFFFF,
	0x00000000
};

/// ベクトル同士の加算
///
/// dst, src: struct BL3D_VECTOR*
///
inline struct BL3D_VECTOR* bl3d_add_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src
)
{
	BL3D_ADD_VECTOR(dst, src);
	
	return dst;
}

/// ベクトル同士の減算
///
/// dst, src: struct BL3D_VECTOR*
///
inline struct BL3D_VECTOR* bl3d_sub_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src
)
{
	BL3D_SUB_VECTOR(dst, src);
	
	return dst;
}

/// ベクトルの各要素同士を乗算したものを合計した値を得る
///
/// (Ax, Ay, Az) *= (Bx, By, Bz)
/// Ax + Ay + Az
inline float bl3d_muladd_vector(
	struct BL3D_VECTOR* a,
	struct BL3D_VECTOR* b
)
{
	float __attribute__((aligned(16))) dst;

	BL3D_MULADD_VECTOR(&dst, a, b);
	
	return dst;
}

/// 転値行列を得る
///
/// A B C    A D G
/// D E F -> B E H
/// G H I    C F I
///
/// （注意：平行移動t[]は計算しません）
inline struct BL3D_MATRIX* bl3d_transpose_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src
)
{
	BL3D_TRANSPOSE_MATRIX(dst, src)

	return dst;
}

/// ２つの行列の積をとります。
/// 計算結果はdstに格納されます。
/// 計算順序は src0 * src1 = dst です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
/// 
/// （注意： src0 または src1 が dst と同じアドレスの場合、不正な計算結果となります！！）
struct BL3D_MATRIX* bl3d_mul_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src0,
	struct BL3D_MATRIX* src1
)
{
	BL3D_MUL_MATRIX(dst, src0, src1);
	
	return dst;
}

/// ベクトルに行列を乗算します。
/// 計算結果はdstに格納されます。
/// 計算順序は m * v = dst です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
inline struct BL3D_VECTOR* bl3d_apply_matrix(
	struct BL3D_VECTOR* dst,
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* v
)
{
	BL3D_APPLY_MATRIX(dst, m, v);
	
	return dst;
}

/// x軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
inline struct BL3D_MATRIX* bl3d_rot_matrix_x(
	struct BL3D_MATRIX* 	m,
	const float		r
)
{
	BL3D_ROT_MATRIX_X(m, r);
	
	return m;
}

/// y軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
inline struct BL3D_MATRIX* bl3d_rot_matrix_y(
	struct BL3D_MATRIX* 	m,
	const float		r
)
{
	BL3D_ROT_MATRIX_Y(m, r);
	
	return m;
}

/// z軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
inline struct BL3D_MATRIX* bl3d_rot_matrix_z(
	struct BL3D_MATRIX* 	m,
	const float		r
)
{
	BL3D_ROT_MATRIX_Z(m, r);
	
	return m;
}

/// 回転角から回転行列を求める。
/// 計算結果は m に格納されます。
/// 回転行列の乗算順序は z * y * x = m です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
inline struct BL3D_MATRIX* bl3d_rot_matrix(
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* r
)
{
	BL3D_ROT_MATRIX(m, r);
	
	return m;
}

/// 平行移動量を行列にセットする。
/// 計算結果は m に格納されます。
inline struct BL3D_MATRIX* bl3d_trans_matrix(
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* v
)
{
	BL3D_TRANS_MATRIX(m, v);
	
	return m;
}

/// 座標変換の合成を行います。
/// bl3d_mul_matrix()とは異なり、平行移動も含めて合成します。
/// 計算結果はdstに格納されます。
///
/// 行列の計算順序は src0.m * src1.m = dst.m です。
/// 平行移動の計算順序は (src0.m * src1.t) + src0.t = dst.t です。
/// つまり src0 が親側で、src1が子側です。
struct BL3D_MATRIX* bl3d_comp_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src0,
	struct BL3D_MATRIX* src1
)
{
	BL3D_COMP_MATRIX(dst, src0, src1);
	
	return dst;
}

///ベクトルのノルムを返す
inline float bl3d_norm_vector(struct BL3D_VECTOR* a)
{
	float __attribute__((aligned(16)))tmp;
	
	BL3D_NORM_VECTOR(&tmp, a);
	
	return tmp;
}

///ベクトルのノルムの逆数を返す
inline float bl3d_invert_norm_vector(struct BL3D_VECTOR* a)
{
	float __attribute__((aligned(16)))tmp;
	
	BL3D_INVERT_NORM_VECTOR(&tmp, a);
	
	return tmp;
}

/// ベクトルの正規化
inline struct BL3D_VECTOR* bl3d_unit_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src
)
{
	BL3D_UNIT_VECTOR(dst, src);
	
	return dst;
}

/// ２ベクトルから外積を得る
inline struct BL3D_VECTOR* bl3d_outer_product_vector(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* src0,
	struct BL3D_VECTOR* src1
)
{
	BL3D_OUTER_PRODUCT(dst, src0, src1);
	
	return dst;
}

/// ベクトル同士の内積を得る
inline float bl3d_inner_product_vector(
	struct BL3D_VECTOR* src0,
	struct BL3D_VECTOR* src1
)
{
	float __attribute__((aligned(16)))tmp;

	BL3D_INNER_PRODUCT_VECTOR(&tmp, src0, src1);

	return tmp;
}

/// 逆行列を得る
struct BL3D_MATRIX* bl3d_invert_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src
)
{
	*dst = bl3d_e_matrix;
	struct BL3D_MATRIX _L = *src;

	struct BL3D_MATRIX* L = &_L;
	struct BL3D_MATRIX* R = dst;


	float a = 1.0 / L->m[0][0];
	L->m[0][0]*=a;	L->m[0][1]*=a;	L->m[0][2]*=a;
	R->m[0][0]*=a;	R->m[0][1]*=a;	R->m[0][2]*=a;

	a = L->m[1][0];
	L->m[1][0]-=L->m[0][0]*a;	L->m[1][1]-=L->m[0][1]*a;	L->m[1][2]-=L->m[0][2]*a;
	R->m[1][0]-=R->m[0][0]*a;	R->m[1][1]-=R->m[0][1]*a;	R->m[1][2]-=R->m[0][2]*a;
	
	a = L->m[2][0];
	L->m[2][0]-=L->m[0][0]*a;	L->m[2][1]-=L->m[0][1]*a;	L->m[2][2]-=L->m[0][2]*a;
	R->m[2][0]-=R->m[0][0]*a;	R->m[2][1]-=R->m[0][1]*a;	R->m[2][2]-=R->m[0][2]*a;

	
	a = 1.0 / L->m[1][1];
			L->m[1][1]*=a;	L->m[1][2]*=a;
	R->m[1][0]*=a;	R->m[1][1]*=a;	R->m[1][2]*=a;

	a = L->m[0][1];
					L->m[0][1]-=L->m[1][1]*a;	L->m[0][2]-=L->m[1][2]*a;
	R->m[0][0]-=R->m[1][0]*a;	R->m[0][1]-=R->m[1][1]*a;	R->m[0][2]-=R->m[1][2]*a;
	
	a = L->m[2][1];
					L->m[2][1]-=L->m[1][1]*a;	L->m[2][2]-=L->m[1][2]*a;
	R->m[2][0]-=R->m[1][0]*a;	R->m[2][1]-=R->m[1][1]*a;	R->m[2][2]-=R->m[1][2]*a;

	
	a = 1.0 / L->m[2][2];
					L->m[2][2]*=a;
	R->m[2][0]*=a;	R->m[2][1]*=a;	R->m[2][2]*=a;

	a = L->m[0][2];
									L->m[0][2]-=L->m[2][2]*a;
	R->m[0][0]-=R->m[2][0]*a;	R->m[0][1]-=R->m[2][1]*a;	R->m[0][2]-=R->m[2][2]*a;
	
	a = L->m[1][2];
									L->m[1][2]-=L->m[2][2]*a;
	R->m[1][0]-=R->m[2][0]*a;	R->m[1][1]-=R->m[2][1]*a;	R->m[1][2]-=R->m[2][2]*a;


	R->t[0] = -L->t[0];	R->t[1] = -L->t[1];	R->t[2] = -L->t[2];


	return dst;	
}

void bl3d_print_matrix(struct BL3D_MATRIX* a)
{
#ifdef __DEBUG__
	g_printf("\nmatrix:\n");
	g_printf("(rotate)\n");
	g_printf("\t[%08f, %08f, %08f]\n",a->m[0][0], a->m[0][1], a->m[0][2]);
	g_printf("\t[%08f, %08f, %08f]\n",a->m[1][0], a->m[1][1], a->m[1][2]);
	g_printf("\t[%08f, %08f, %08f]\n",a->m[2][0], a->m[2][1], a->m[2][2]);
	g_printf("(transfer)\n");
	g_printf("\t[%08f, %08f, %08f]\n\n",a->t[0], a->t[1], a->t[2]);
#endif // __DEBUG__
}
