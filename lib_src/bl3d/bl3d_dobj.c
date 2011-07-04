//#define __DEBUG__

#include "bl3d.h"

/// ３次元オブジェクトハンドラの初期設定。
void bl3d_link_object(
	struct BL3D_DOBJ* 	  	dobj,
	struct BL3D_TRIANGLE_G_T*	model_data,
	const int			model_data_len
)
{
	dobj->model_data	= model_data;
	dobj->model_data_len	= model_data_len;
	
	dobj->local_coord.compleate_flg	= 0;
	dobj->local_coord.rotate 	= bl3d_0_vector;
	dobj->local_coord.transfer 	= bl3d_0_vector;
	dobj->local_coord.super		= NULL;
}

/// オーダリングテーブルにオブジェクトを割り付ける。
/// この関数によって、dobjの各ポリゴンにジオメトリー演算が行われ、
/// オーダリングテーブルに各ポリゴンの最終的な2次元描画パケットを登録する。
void bl3d_sort_object(
	struct BL3D_DOBJ*	dobj,
	struct BL3D_OT*		ot
)
{
	struct BL3D_MATRIX lw;
	bl3d_get_lws(&lw, &bl3d_ls_matrix, &dobj->local_coord);
	
	struct BL3D_TRIANGLE_G_T* p = dobj->model_data;
	const int len = dobj->model_data_len;

	
#ifdef __DEBUG__
	bl3d_ws_matrix = bl3d_e_matrix;
	bl3d_ls_matrix = bl3d_e_matrix;
	g_printf("ws ");
	bl3d_print_matrix(&bl3d_ws_matrix);
	g_printf("ls ");
	bl3d_print_matrix(&bl3d_ls_matrix);
#endif // __DEBUG__

	
	int i;
	for(i = 0; i < len; i++) {
		bl3d_sort_triangle_g_t(&p[i], ot);
	}
}
