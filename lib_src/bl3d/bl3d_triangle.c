//#define __DEBUG__

#include "blike.h"
#include "bl3d.h"
#include "bl3d_matrix_macro.h"
#include "bl3d_io_macro.h"

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

static float bl3d_get_big(const float a, const float b, const float c)
{
	float z = a;
	
	if(z < b) {
		z = b;
	}
	
	if(z < c) {
		z = c;
	}
	
	return z;
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
		BL3D_APPLY_MATRIX(&ot_tag->vertex[i], &bl3d_ls_matrix, &a->vertex[i]);
		
		BL3D_ADD_VECTOR(&ot_tag->vertex[i], (struct BL3D_VECTOR*)bl3d_ls_matrix.t);
		ot_tag->vertex[i].z *= bl3d_ot_scale;
		
		const float a = (1.0 / ot_tag->vertex[i].z) * bl3d_ot_projection;
		
		ot_tag->vertex[i].x *= a;
		ot_tag->vertex[i].y *= a;
	}
	
	
	
	int big_z = (int)bl3d_get_big(
		ot_tag->vertex[0].z, ot_tag->vertex[1].z, ot_tag->vertex[2].z
	);
	
	if(big_z >= BL3D_OT_LENGTH - 1) {
		return;
	}
	
	
	
	for(i = 0; i < 3; i++) {
		struct BL3D_VECTOR vertex = {
			.x = ot_tag->vertex[i].x, .y = ot_tag->vertex[i].y, .z = 0
		};

		float r;
		BL3D_NORM_VECTOR(&r, &vertex);
		if(r < bl3d_screen_radius) {
			break;
		}
	}
	
	if(i >= 3) {
		return;
	}
	
	
	
	int min_z = (int)bl3d_get_min(
		ot_tag->vertex[0].z, ot_tag->vertex[1].z, ot_tag->vertex[2].z
	);

	if(min_z < bl3d_ot_projection) {
		return;
	}
	
	const int z = (min_z >= BL3D_OT_LENGTH)? BL3D_OT_LENGTH - 1: min_z;
	
	
	
	struct BL3D_VECTOR normal_vector;
	BL3D_GET_NORMAL_TRIANGLE(&normal_vector, &ot_tag->vertex[0], &ot_tag->vertex[1], &ot_tag->vertex[2]);
	if(normal_vector.z < 0) {
		return;
	}
	
	
	
	for(i = 0; i < 3; i++) {
		BL3D_INNER_CUBE_VERTEX(
			&ot_tag->vertex[i],
			&ot_tag->vertex[i],
			&bl3d_screen_cube_min,
			&bl3d_screen_cube_max
		);
	}
	
	
	
	for(i = 0; i < 3; i++) {
		BL3D_MUL2_VECTOR(
			(struct BL3D_VECTOR*)&ot_tag->color[i],
			(struct BL3D_VECTOR*)&a->color[i],
			(struct BL3D_VECTOR*)&bl3d_ambient_depth
		)

		ot_tag->texture[i] = a->texture[i];

		ot_tag->vertex[i].x += bl3d_screen_offset[0];
		ot_tag->vertex[i].y += bl3d_screen_offset[1];
	}



	ot_tag->base_color = bl3d_0_cvector;
	
	for(i = 0; i < 3; i++) {
		if(bl3d_system_flat_light_use_flag[i] == TRUE) {
			float power;
			BL3D_INNER_PRODUCT_VECTOR(
				&power, 
				&normal_vector,
				&bl3d_system_flat_light[i].vector
			);
			if(power < 0) {
				power = 0;
			}
			
			struct BL3D_VECTOR power_vector = {power, power, power, 0};

			struct BL3D_VECTOR tmp;
			BL3D_MUL2_VECTOR(
				&tmp,
				(struct BL3D_VECTOR*)&bl3d_system_flat_light[i].color,
				&power_vector
			);
			
			BL3D_ADD_VECTOR(
				(struct BL3D_VECTOR*)&ot_tag->base_color,
				&tmp
			);
		}
	}
	
	
	
	ot_tag->next = NULL;

	if(ot->ot_tag_top[z] != NULL) {
		ot->ot_tag_tail[z]->next = ot_tag;
		ot->ot_tag_tail[z]	 = ot_tag;
	}
	else {
		ot->ot_tag_top[z]   = ot_tag;
		ot->ot_tag_tail[z]  = ot_tag;
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
	struct BL3D_CVECTOR* BC,
	struct BL3D_CVECTOR* BASE_C
)
{
	struct BL3D_VECTOR L;
	BL3D_DIFF_VECTOR(&L, B, A);
	L.z = 0;
	
	float r;
	BL3D_NORM_VECTOR(&r, &L);

	float ir;
	BL3D_INVERT_NORM_VECTOR(&ir, &L);
	
	struct BL3D_VECTOR ir_vec = {ir, ir, ir, 0};
	
	struct BL3D_VECTOR U;
	BL3D_MUL2_VECTOR(&U, &L, &ir_vec);

	struct BL3D_VECTOR P = *A;

	
	
	struct BL3D_VECTOR LT;
	BL3D_DIFF_VECTOR(&LT, BT, AT);
	LT.z = 0;
	
	struct BL3D_VECTOR UT;
	BL3D_MUL2_VECTOR(&UT, &LT, &ir_vec);
	
	struct BL3D_VECTOR PT = *AT;
	
	
	
	struct BL3D_CVECTOR LC;
	BL3D_DIFF_VECTOR((struct BL3D_VECTOR*)&LC, (struct BL3D_VECTOR*)BC, (struct BL3D_VECTOR*)AC);
	
	struct BL3D_CVECTOR UC;
	BL3D_MUL2_VECTOR((struct BL3D_VECTOR*)&UC, (struct BL3D_VECTOR*)&LC, &ir_vec);
	
	struct BL3D_CVECTOR PC = *AC;
	
	
	
	int i;
	for(i = 0; i < r; i++) {
		int x = (int)P.x;
		int y = (int)P.y;
		
		
		int tx = (int)PT.x;
		int ty = (int)PT.y;
		
		
		int _C;
		BL3D_GET_PIX(tx, ty, _C, texture_vram);
		int _Cr = (_C >> 16) & 0xFF;
		int _Cg = (_C >> 8 ) & 0xFF;
		int _Cb = (_C      ) & 0xFF;
		static const float i255 = 1.0 / 255;
		struct BL3D_CVECTOR C = {
			.r = ((float)_Cr) * i255,
			.g = ((float)_Cg) * i255,
			.b = ((float)_Cb) * i255
		};
		
		
		BL3D_MUL_VECTOR((struct BL3D_VECTOR*)&C, (struct BL3D_VECTOR*)&PC);
		BL3D_ADD_VECTOR((struct BL3D_VECTOR*)&C, (struct BL3D_VECTOR*)BASE_C);		
		
		
		(bl3d_system_frame_buffer->y_offset_table[y])[x] 	= C;
		
		
		BL3D_ADD_VECTOR(&P, &U);
		BL3D_ADD_VECTOR(&PT, &UT);
		BL3D_ADD_VECTOR((struct BL3D_VECTOR*)&PC, (struct BL3D_VECTOR*)&UC);
	}
}

static int bl3d_get_index_min(const float a, const float b, const float c)
{
	float z = a;
	int i = 0;
	
	if(z > b) {
		z = b;
		i = 1;
	}
	
	if(z > c) {
		z = c;
		i = 2;
	}
	
	return i;
}

static int bl3d_get_index_max(const float a, const float b, const float c)
{
	float z = a;
	int i = 0;
	
	if(z < b) {
		z = b;
		i = 1;
	}
	
	if(z < c) {
		z = c;
		i = 2;
	}
	
	return i;
}

static void bl3d_get_index(
	int* min,
	int* mid,
	int* max,
	const float a,
	const float b,
	const float c
)
{
	*min = bl3d_get_index_min(a, b, c);
	*max = bl3d_get_index_max(a, b, c);
	
	switch(*min) {
	case 0:
		switch(*max) {
		case 1: *mid = 2; break;
		case 2: *mid = 1; break;

		case 0: *mid = 0; break;
		}
		break;
		
	case 1:
		switch(*max) {
		case 0: *mid = 2; break;
		case 2: *mid = 0; break;

		case 1: *mid = 1; break;
		}
		break;
		
	case 2:
		switch(*max) {
		case 0: *mid = 1; break;
		case 1: *mid = 0; break;

		case 2: *mid = 2; break;
		}
		break;
	}
}

// x軸に平行な線で、三角形を２つに分割する。
// 結果は２つの BL3D_OT_TAG 型として返される
//
// 再構築される三角形は、それぞれ、元の時計回りに準じた形で、
// 分割線にの両端がA,Cで、分割線の向かいの点がBとなるように再構築される。
static void bl3d_xline_divide_triangle_g_t(
	struct BL3D_OT_TAG* dstA,
	struct BL3D_OT_TAG* dstB,
	struct BL3D_OT_TAG* src
)
{
	int min, mid, max;
	bl3d_get_index(
		&min, &mid, &max,
		src->vertex[0].y, src->vertex[1].y, src->vertex[2].y
	);
	
	
	
	float vumid = src->vertex[mid].y - src->vertex[min].y;
 	float vumax = src->vertex[max].y - src->vertex[min].y;
	float vuy = (vumax != 0)? vumid / vumax: 0;
	struct BL3D_VECTOR VUY = {vuy, vuy, vuy, 0};
	
	
	 
	struct BL3D_VECTOR VL;
	BL3D_DIFF_VECTOR(&VL, &src->vertex[max], &src->vertex[min]);
	
	struct BL3D_VECTOR VP;
	BL3D_MUL2_VECTOR(&VP, &VL, &VUY);
	BL3D_ADD_VECTOR(&VP, &src->vertex[min]);
	
	
	
	struct BL3D_VECTOR TL;
	BL3D_DIFF_VECTOR(&TL, &src->texture[max], &src->texture[min]);

	struct BL3D_VECTOR TP;
	BL3D_MUL2_VECTOR(&TP, &TL, &VUY);
	BL3D_ADD_VECTOR(&TP, &src->texture[min]);
	
	
	
	struct BL3D_CVECTOR CL;
	BL3D_DIFF_VECTOR(
		(struct BL3D_VECTOR*)&CL,
		(struct BL3D_VECTOR*)&src->color[max],
		(struct BL3D_VECTOR*)&src->color[min]
	);
	
	struct BL3D_CVECTOR CP;
	BL3D_MUL2_VECTOR(
		(struct BL3D_VECTOR*)&CP,
		(struct BL3D_VECTOR*)&CL,
		&VUY
	);
	BL3D_ADD_VECTOR(
		(struct BL3D_VECTOR*)&CP,
		(struct BL3D_VECTOR*)&src->color[min]
	);
	
	
	
	dstA->type = dstB->type = BL3D_TRIANGLE_TYPE_G_T;
	dstA->texture_vram = dstB->texture_vram = src->texture_vram;
	dstA->base_color = dstB->base_color = src->base_color;

	dstA->vertex[0]  = dstB->vertex[0]  = VP;
	dstA->texture[0] = dstB->texture[0] = TP;
	dstA->color[0]   = dstB->color[0]   = CP;

	switch(mid) {
	case 0:
		dstA->vertex[1] = src->vertex[2];
		dstA->vertex[2] = src->vertex[0];

		dstA->texture[1] = src->texture[2];
		dstA->texture[2] = src->texture[0];
	
		dstA->color[1] = src->color[2];
		dstA->color[2] = src->color[0];
		
		
		dstB->vertex[1] = src->vertex[0];
		dstB->vertex[2] = src->vertex[1];

		dstB->texture[1] = src->texture[0];
		dstB->texture[2] = src->texture[1];
	
		dstB->color[1] = src->color[0];
		dstB->color[2] = src->color[1];

		break;
	
	case 1:
		dstA->vertex[1] = src->vertex[0];
		dstA->vertex[2] = src->vertex[1];

		dstA->texture[1] = src->texture[0];
		dstA->texture[2] = src->texture[1];
			
		dstA->color[1] = src->color[0];
		dstA->color[2] = src->color[1];
		
		
		dstB->vertex[1] = src->vertex[1];
		dstB->vertex[2] = src->vertex[2];

		dstB->texture[1] = src->texture[1];
		dstB->texture[2] = src->texture[2];
			
		dstB->color[1] = src->color[1];
		dstB->color[2] = src->color[2];

		break;
	
	case 2:
		dstA->vertex[1] = src->vertex[1];
		dstA->vertex[2] = src->vertex[2];
		
		dstA->texture[1] = src->texture[1];
		dstA->texture[2] = src->texture[2];
		
		dstA->color[1] = src->color[1];
		dstA->color[2] = src->color[2];
		
		
		dstB->vertex[1] = src->vertex[2];
		dstB->vertex[2] = src->vertex[0];
		
		dstB->texture[1] = src->texture[2];
		dstB->texture[2] = src->texture[0];
		
		dstB->color[1] = src->color[2];
		dstB->color[2] = src->color[0];

		break;
	}
}

static void __bl3d_draw_triangle_g_t(struct BL3D_OT_TAG* a)
{
	struct BL3D_VECTOR A;
	BL3D_DIFF_VECTOR(&A, &a->vertex[1], &a->vertex[0]);
	A.z = 0;
	
	struct BL3D_VECTOR B;
	BL3D_DIFF_VECTOR(&B, &a->vertex[1], &a->vertex[2]);
	B.z = 0;
	
	float ar;
	BL3D_NORM_VECTOR(&ar, &A);

	float br;
	BL3D_NORM_VECTOR(&br, &B);
	
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

	float ilr;
	BL3D_INVERT_NORM_VECTOR(&ilr, L);

	struct BL3D_VECTOR ilr_vector = {ilr, ilr, ilr, 0};

	
	struct BL3D_VECTOR LU;
	BL3D_MUL2_VECTOR(&LU, L, &ilr_vector);
	
	struct BL3D_VECTOR SU;
	BL3D_MUL2_VECTOR(&SU, S, &ilr_vector);
	
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
	
	struct BL3D_VECTOR AT;
	BL3D_DIFF_VECTOR(&AT, &a->texture[1], &a->texture[0]);
	
	struct BL3D_VECTOR BT;
	BL3D_DIFF_VECTOR(&BT, &a->texture[1], &a->texture[2]);
	
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
	
	struct BL3D_VECTOR LTU;
	BL3D_MUL2_VECTOR(&LTU, LT, &ilr_vector);
	
	struct BL3D_VECTOR STU;
	BL3D_MUL2_VECTOR(&STU, ST, &ilr_vector);
	
	
	
	struct BL3D_CVECTOR AC;
	BL3D_DIFF_VECTOR((struct BL3D_VECTOR*)&AC, (struct BL3D_VECTOR*)&a->color[1], (struct BL3D_VECTOR*)&a->color[0]);

	struct BL3D_CVECTOR BC;
	BL3D_DIFF_VECTOR((struct BL3D_VECTOR*)&BC, (struct BL3D_VECTOR*)&a->color[1], (struct BL3D_VECTOR*)&a->color[2]);
	
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
	
	struct BL3D_CVECTOR LCU;
	BL3D_MUL2_VECTOR((struct BL3D_VECTOR*)&LCU, (struct BL3D_VECTOR*)LC, &ilr_vector);

	struct BL3D_CVECTOR SCU;
	BL3D_MUL2_VECTOR((struct BL3D_VECTOR*)&SCU, (struct BL3D_VECTOR*)SC, &ilr_vector);

	
	
	int i;
	const int _lr = (int)lr;
	for(i = 0; i < _lr; i++) {
		bl3d_draw_line_g_t(
			&LP, &SP,
			&LTP, &STP, texture_vram,
			&LCP, &SCP,
			&a->base_color
		);

		
		BL3D_ADD_VECTOR(&LP, &LU);
		BL3D_ADD_VECTOR(&SP, &SU);
		
		BL3D_ADD_VECTOR(&LTP, &LTU);
		BL3D_ADD_VECTOR(&STP, &STU);
		
		BL3D_ADD_VECTOR((struct BL3D_VECTOR*)&LCP, (struct BL3D_VECTOR*)&LCU);
		BL3D_ADD_VECTOR((struct BL3D_VECTOR*)&SCP, (struct BL3D_VECTOR*)&SCU);

		
#ifdef __DEBUG__
		wait(1000/60);
#endif // __DEBUG__
	}
}

void bl3d_draw_triangle_g_t(struct BL3D_OT_TAG* a)
{
	struct BL3D_OT_TAG A, B;
	bl3d_xline_divide_triangle_g_t(&A, &B, a);
	
	__bl3d_draw_triangle_g_t(&A);
	__bl3d_draw_triangle_g_t(&B);
}
