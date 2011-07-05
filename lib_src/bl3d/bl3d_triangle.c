//#define __DEBUG__

#include "blike.h"
#include "bl3d.h"

void bl3d_init_triangle_g_t(
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
)
{
	a->type = BL3D_TRIANGLE_TYPE_G_T;
	
	a->vertex[0] = *vertex0;
	a->vertex[1] = *vertex1;
	a->vertex[2] = *vertex2;

	a->texture[0]	= *texture0;
	a->texture[1]	= *texture1;
	a->texture[2]	= *texture2;
	a->texture_vram	= texture_vram;
	
	a->color[0] = *color0;
	a->color[1] = *color1;
	a->color[2] = *color2;
}

static float bl3d_get_min(const float a, const float b, const float c)
{
	float z = a;
	
	if(z > b) {
		z = b;
	}
	
	if(z > c) {
		z = c;
	}
	
	return z;
}

/// 三角形の頂点ベクトルから法線ベクトルを得る。
struct BL3D_VECTOR* bl3d_get_normal_triangle(
	struct BL3D_VECTOR* dst,
	struct BL3D_VECTOR* vertex0,
	struct BL3D_VECTOR* vertex1,
	struct BL3D_VECTOR* vertex2
)
{
	struct BL3D_VECTOR A = {
		.x = vertex1->x - vertex0->x,
		.y = vertex1->y - vertex0->y,
		.z = vertex1->z - vertex0->z
	};

	struct BL3D_VECTOR B = {
		.x = vertex2->x - vertex0->x,
		.y = vertex2->y - vertex0->y,
		.z = vertex2->z - vertex0->z
	};

	bl3d_outer_product_vector(dst, &A, &B);

	return dst;
}

/// テクスチャー・グロー三角形を、オーダリングテーブルに割り当てる。
/// 手順としては、まずBL3D_TRIANGLE_G_Tからot_tagへ変換し、それをotに登録する。
///
/// このot_tagのメモリー領域は、各ポリゴンにユニークでなければならない。
/// たとえば BL3D_DOBJ が全部で１００ポリゴンだとすれば、表示のために１００個のot_tag領域が必要。
///
/// ot_tagはユニークである必要があるが、BL3D_TRIANGLE_G_Tなどのポリゴンのオリジナルデータは
/// 必ずしもユニークである必要はない（共有してもよい）
/// たとえば１枚のBL3D_TRIANGLE_G_Tを、複数のot_tagに割り当てて、複数表示することは可能。
///
/// a: オリジナルのポリゴンデータのアドレス
/// ot: ポリゴンの登録先のオーダリングテーブル
void bl3d_sort_triangle_g_t(
	struct BL3D_TRIANGLE_G_T*	a,
	struct BL3D_OT*			ot
)
{
	int i;

	struct BL3D_OT_TAG* ot_tag = bl3d_rental_ot_tag();
	
	ot_tag->type = BL3D_TRIANGLE_TYPE_G_T;

	ot_tag->texture_vram = a->texture_vram;

	
	
	for(i = 0; i < 3; i++) {
		bl3d_apply_matrix(&ot_tag->vertex[i], &bl3d_ls_matrix, &a->vertex[i]);
		
		ot_tag->vertex[i].x += bl3d_ls_matrix.t[0];
		ot_tag->vertex[i].y += bl3d_ls_matrix.t[1];
		ot_tag->vertex[i].z += bl3d_ls_matrix.t[2];
	}
	
#ifdef __DEBUG__
	g_printf(
		"V0[%f, %f, %f] V1[%f, %f, %f] V2[%f, %f, %f]\n",
		V[0].x, V[0].y, V[0].z,
		V[1].x, V[1].y, V[1].z,
		V[2].x, V[2].y, V[2].z
	);
#endif // __DEBUG__
		
	

	for(i = 0; i < 3; i++) {
		ot_tag->texture[i] = a->texture[i];
		ot_tag->color[i]   = a->color[i];
	}
	
	
	
	int z = (int)bl3d_get_min(
		ot_tag->vertex[0].z, ot_tag->vertex[1].z, ot_tag->vertex[2].z
	);
	
	z &= BL3D_OT_LENGTH - 1;		// 0x7FFF
	
	ot_tag->next = NULL;

	if(ot->ot_tag_top[z] == NULL) {
		ot->ot_tag_top[z]   = ot_tag;
		ot->ot_tag_tail[z]  = ot_tag;
	}
	else {
		ot->ot_tag_tail[z]->next = ot_tag;
		ot->ot_tag_tail[z]	 = ot_tag;
	}
}

static float bl3d_get_max(const float a, const float b)
{
	if(a >= b) {
		return a;
	}
	else {
		return b;
	}
}

static void bl3d_draw_line_g_t(
	struct BL3D_VECTOR* A,
	struct BL3D_VECTOR* B,
	struct BL3D_VECTOR* AT,
	struct BL3D_VECTOR* BT,
	const int texture_vram,
	struct BL3D_CVECTOR* AC,
	struct BL3D_CVECTOR* BC
)
{
	struct BL3D_VECTOR L = {
		.x = B->x - A->x,
		.y = B->y - A->y
	};
	
	float r = bl3d_sqrt(L.x * L.x + L.y * L.y);
	
	struct BL3D_VECTOR U = {
		.x = L.x / r,
		.y = L.y / r
	};

	struct BL3D_VECTOR P = *A;

	
	
	struct BL3D_VECTOR LT = {
		.x = BT->x - AT->x,
		.y = BT->y - AT->y,
	};
	
	struct BL3D_VECTOR UT = {
		.x = LT.x / r,
		.y = LT.y / r
	};
	
	struct BL3D_VECTOR PT = *AT;
	
	
	
	struct BL3D_CVECTOR LC = {
		.r = BC->r - AC->r,
		.g = BC->g - AC->g,
		.b = BC->b - AC->b,
	};
	
	struct BL3D_CVECTOR UC = {
		.r = LC.r / r,
		.g = LC.g / r,
		.b = LC.b / r
	};
	
	struct BL3D_CVECTOR PC = *AC;
	
	
	
	int i;
	for(i = 0; i < r; i++) {
		int x = (int)P.x;
		int y = (int)P.y;
		
		
		int tx = (int)PT.x;
		int ty = (int)PT.y;

		
		slctWin(texture_vram);
		int C = bl_getPix(tx+0, ty+0);
		
		float Cr = (C >> 16) & 0xFF;
		float Cg = (C >> 8 ) & 0xFF;
		float Cb = (C >> 0 ) & 0xFF;

		int col_r = Cr * PC.r;
		int col_g = Cg * PC.g;
		int col_b = Cb * PC.b;
		
		col_r = (col_r > 255)? 255: col_r;
		col_g = (col_g > 255)? 255: col_g;
 		col_b = (col_b > 255)? 255: col_b;
		
		int col = (col_r << 16) | (col_g << 8) | (col_b << 0);
		
		
		slctWin(0);
		bl_setPix(x+0, y+0, col);
//		bl_setPix(x+1, y+0, col);
//		bl_setPix(x+0, y+1, col);
//		bl_setPix(x+1, y+1, col);
		
		
		P.x += U.x;
		P.y += U.y;
		
		PT.x += UT.x;
		PT.y += UT.y;
		
		PC.r += UC.r;
		PC.g += UC.g;
		PC.b += UC.b;
	}
}

void bl3d_draw_triangle_g_t(struct BL3D_OT_TAG* a)
{
	struct BL3D_VECTOR A = {
		.x = a->vertex[1].x - a->vertex[0].x,
		.y = a->vertex[1].y - a->vertex[0].y
	};

	struct BL3D_VECTOR B = {
		.x = a->vertex[1].x - a->vertex[2].x,
		.y = a->vertex[1].y - a->vertex[2].y
	};
	
	float ar = bl3d_sqrt(A.x * A.x + A.y * A.y);
	float br = bl3d_sqrt(B.x * B.x + B.y * B.y);
	
	float lr;
	struct BL3D_VECTOR* L;
	struct BL3D_VECTOR* S;
	struct BL3D_VECTOR LP;
	struct BL3D_VECTOR SP;
	if(ar >= br) {
		lr = ar;
		L = &A;
		S = &B;
		LP = a->vertex[0];
		SP = a->vertex[2];
	}
	else {
		lr = br;
		L = &B;
		S = &A;
		LP = a->vertex[2];
		SP = a->vertex[0];
	}
	
	struct BL3D_VECTOR LU = {
		.x = L->x / lr,
		.y = L->y / lr
	};
	
	struct BL3D_VECTOR SU = {
		.x = S->x / lr,
		.y = S->y / lr
	};
	
#ifdef __DEBUG__
	g_printf(
		"LU[%f, %f] SU[%f, %f] L[%f, %f] S[%f, %f] LP[%f, %f] SP[%f, %f]\n",
		LU.x, LU.y,
		SU.x, SU.y,
		L->x, L->y,
		S->x, S->y,
		LP.x, LP.y,
		SP.x, SP.y
	);
#endif // __DEBUG__
	
	
	
	const int texture_vram = a->texture_vram;
	
	struct BL3D_VECTOR AT = {
		.x = a->texture[1].x - a->texture[0].x,
		.y = a->texture[1].y - a->texture[0].y,
	};
	
	struct BL3D_VECTOR BT = {
		.x = a->texture[1].x - a->texture[2].x,
		.y = a->texture[1].y - a->texture[2].y,
	};
	
	struct BL3D_VECTOR* LT;
	struct BL3D_VECTOR* ST;
	struct BL3D_VECTOR LTP;
	struct BL3D_VECTOR STP;
	if(ar >= br) {
		LT = &AT;
		ST = &BT;
		LTP = a->texture[0];
		STP = a->texture[2];
	}
	else {
		LT = &BT;
		ST = &AT;
		LTP = a->texture[2];
		STP = a->texture[0];
	}
	
	struct BL3D_VECTOR LTU = {
		.x = LT->x / lr,
		.y = LT->y / lr
	};
	
	struct BL3D_VECTOR STU = {
		.x = ST->x / lr,
		.y = ST->y / lr
	};
	
	
	
	struct BL3D_CVECTOR AC = {
		.r = a->color[1].r - a->color[0].r,
		.g = a->color[1].g - a->color[0].g,
		.b = a->color[1].b - a->color[0].b,
	};

	struct BL3D_CVECTOR BC = {
		.r = a->color[1].r - a->color[2].r,
		.g = a->color[1].g - a->color[2].g,
		.b = a->color[1].b - a->color[2].b,
	};
	
	struct BL3D_CVECTOR* LC;
	struct BL3D_CVECTOR* SC;
	struct BL3D_CVECTOR LCP;
	struct BL3D_CVECTOR SCP;
	if(ar >= br) {
		LC = &AC;
		SC = &BC;
		LCP = a->color[0];
		SCP = a->color[2];
	}
	else {
		LC = &BC;
		SC = &AC;
		LCP = a->color[2];
		SCP = a->color[0];
	}
	
	struct BL3D_CVECTOR LCU = {
		.r = LC->r / lr,
		.g = LC->g / lr,
		.b = LC->b / lr
	};

	struct BL3D_CVECTOR SCU = {
		.r = SC->r / lr,
		.g = SC->g / lr,
		.b = SC->b / lr
	};

	
	
	int i;
	const int _lr = (int)lr;
	for(i = 0; i < _lr; i++) {
		bl3d_draw_line_g_t(
			&LP, &SP,
			&LTP, &STP, texture_vram,
			&LCP, &SCP
		);

		
		LP.x += LU.x;
		LP.y += LU.y;
		
		SP.x += SU.x;
		SP.y += SU.y;
		
		
		LTP.x += LTU.x;
		LTP.y += LTU.y;
		
		STP.x += STU.x;
		STP.y += STU.y;
		
		
		LCP.r += LCU.r;
		LCP.g += LCU.g;
		LCP.b += LCU.b;

		SCP.r += SCU.r;
		SCP.g += SCU.g;
		SCP.b += SCU.b;

		
#ifdef __DEBUG__
		wait(1000/100);
#endif // __DEBUG__
	}
}
