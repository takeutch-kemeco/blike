#include "bl3d.h"

/// ２つの行列の積をとります。
/// 計算結果はdstに格納されます。
/// 計算順序は src0 * src1 = dst です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
struct BL3D_MATRIX* bl3d_mul_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src0,
	struct BL3D_MATRIX* src1
)
{
	int j, i;
	
	for(j=0; j<3; j++) {
		for(i=0; i<3; i++) {
			dst->m[j][i] =
				src0->m[j][0] * src1->m[0][i] +
				src0->m[j][1] * src1->m[1][i] +
				src0->m[j][2] * src1->m[2][i];
		}
	}
	
	return dst;
}

/// ベクトルに行列を乗算します。
/// 計算結果はdstに格納されます。
/// 計算順序は m * v = dst です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
struct BL3D_VECTOR* bl3d_apply_matrix(
	struct BL3D_VECTOR* dst,
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* v
)
{
	float* p = &(v->x);
	float* q = &(dst->x);

	int j;
	
	for(j=0; j<3; j++) {
		q[j] =
			m->m[j][0] * p[0] +
			m->m[j][1] * p[1] +
			m->m[j][2] * p[2];
	}
	
	return dst;
}

/// x軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
struct BL3D_MATRIX* bl3d_rot_matrix_x(
	struct BL3D_MATRIX* 	m,
	float			r
)
{
	float s = bl3d_sin(r);
	float c = bl3d_cos(r);
	
	m->m[0][0] = 1;	m->m[0][1] = 0; m->m[0][2] = 0;
	m->m[1][0] = 0;	m->m[1][1] = c; m->m[1][2] = -s;
	m->m[2][0] = 0;	m->m[2][1] = s; m->m[2][2] = c;
	
	return m;
}

/// y軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
struct BL3D_MATRIX* bl3d_rot_matrix_y(
	struct BL3D_MATRIX* 	m,
	float			r
)
{
	float s = bl3d_sin(r);
	float c = bl3d_cos(r);
	
	m->m[0][0] = c;	 m->m[0][1] = 0; m->m[0][2] = s;
	m->m[1][0] = 0;	 m->m[1][1] = 1; m->m[1][2] = 0;
	m->m[2][0] = -s; m->m[2][1] = 0; m->m[2][2] = c;
	
	return m;
}

/// z軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
struct BL3D_MATRIX* bl3d_rot_matrix_z(
	struct BL3D_MATRIX* 	m,
	float			r
)
{
	float s = bl3d_sin(r);
	float c = bl3d_cos(r);
	
	m->m[0][0] = c;	m->m[0][1] = -s; m->m[0][2] = 0;
	m->m[1][0] = s;	m->m[1][1] = c;  m->m[1][2] = 0;
	m->m[2][0] = 0;	m->m[2][1] = 0;  m->m[2][2] = 1;
	
	return m;
}

/// 回転角から回転行列を求める。
/// 計算結果は m に格納されます。
/// 回転行列の乗算順序は z * y * x = m です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
struct BL3D_MATRIX* bl3d_rot_matrix(
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* r
)
{
	struct BL3D_MATRIX mx, my, mz, tmp;

	bl3d_rot_matrix_x(&mx, r->x);
	bl3d_rot_matrix_y(&my, r->y);
	bl3d_rot_matrix_z(&mz, r->z);
	
	bl3d_mul_matrix(&tmp, &mz, &my);
	bl3d_mul_matrix(m, &tmp, &mx);
	
	return m;
}

/// 平行移動量を行列にセットする。
/// 計算結果は m に格納されます。
struct BL3D_MATRIX* bl3d_trans_matrix(
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* v
)
{
	float* p = &(v->x);
	m->t[0] = p[0];
	m->t[1] = p[1];
	m->t[2] = p[2];
	
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
	bl3d_mul_matrix(dst, src0, src1);

	bl3d_apply_matrix(
		(struct BL3D_VECTOR*)(dst->t),
		src0,
		(struct BL3D_VECTOR*)(src1->t)
	);
	
	dst->t[0] += src0->t[0];
	dst->t[1] += src0->t[1];
	dst->t[2] += src0->t[2];
	
	return dst;
}

void bl3d_print_matrix(struct BL3D_MATRIX* a)
{
	g_printf("\nmatrix:\n");
	g_printf("(rotate)\n");
	g_printf("\t[%08f, %08f, %08f]\n",a->m[0][0], a->m[0][1], a->m[0][2]);
	g_printf("\t[%08f, %08f, %08f]\n",a->m[1][0], a->m[1][1], a->m[1][2]);
	g_printf("\t[%08f, %08f, %08f]\n",a->m[2][0], a->m[2][1], a->m[2][2]);
	g_printf("(transfer)\n");
	g_printf("\t[%08f, %08f, %08f]\n\n",a->t[0], a->t[1], a->t[2]);
}
