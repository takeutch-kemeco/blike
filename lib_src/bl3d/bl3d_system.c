//#define __DEBUG__

#include "bl3d.h"

/// 画面の中心位置のXY座標
/// これを使って、画面中心を（０、０）として考える
int bl3d_screen_offset[2];

/// 投影面のZ座標位置の設定
/// デフォルトは1000
int bl3d_screen_projection = 1000;

/// 単位行列
const struct BL3D_MATRIX bl3d_e_matrix = {
	.m[0][0] = 1, .m[0][1] = 0, .m[0][2] = 0,
	.m[1][0] = 0, .m[1][1] = 1, .m[1][2] = 0,
	.m[2][0] = 0, .m[2][1] = 0, .m[2][2] = 1,
	.t[0] = 0, .t[1] = 0, .t[2] = 0
};

/// 0ベクトル
const struct BL3D_VECTOR bl3d_0_vector = {.x = 0, .y = 0, .z = 0, .pad = 0};

/// ワールドからスクリーンへの行列
struct BL3D_MATRIX bl3d_ws_matrix;

/// ローカルからスクリーンへの行列。
/// 主に各オブジェクトの計算時のテンポラリ変数として用いる。
struct BL3D_MATRIX bl3d_ls_matrix;

/// システムの環境光。
/// 光の当たってない部分が、光の当たってる部分に対してどれだけ減衰するかの割合を設定する。
/// デフォルトでは各色 1/4 となる。
struct BL3D_CVECTOR bl3d_ambient_depth = {
	.r = 1.0/4, .g = 1.0/4, .b = 1.0/4
};

/// システムの平行光源のベクトルと色。
/// 平行光源は３個まで使用できる。
struct BL3D_VECTOR bl3d_flat_light_vector[3];
struct BL3D_CVECTOR bl3d_flat_light_color[3];

/// システムの平行光源の、ローカルtoワールド行列。
/// 平行光源をワールド座標を用いて設定するのであれば、単位行列でいい。
/// ３光源で同じ座標系を共有する。
struct BL3D_MATRIX bl3d_flat_light_matrix;

/// システムのワールドtoスクリーン行列を設定する。
/// BL3D_VIEWの親座標をワールド以外に設定した場合は、
/// 親座標系の値の変化した場合、視点も影響を受けるため、この関数を毎フレーム実行する必要がある。
void bl3d_set_view(struct BL3D_VIEW* pv)
{
	bl3d_ws_matrix = bl3d_e_matrix;
}

/// システムの平行光源を設定する。
/// 平行光源は３個まで使用できる。
///
/// lt: 光源情報
/// id: 光源番号(0 ~ 2)
void bl3d_set_flat_light(struct BL3D_FLAT_LIGHT* lt, int id)
{
	id &= 0x03;
	
	bl3d_flat_light_color[id] = lt->color;
	
	struct BL3D_VECTOR tmp = lt->vector;
	bl3d_apply_matrix(&bl3d_flat_light_vector[id], &bl3d_ws_matrix, &tmp);
}

/// ローカル座標系の初期化
/// base: 初期化する座標系へのポインタ
/// super: 親座標系へのポインタ。NULLの場合はbaseをワールド座標系と考える。
void bl3d_init_coordinate(
	struct BL3D_COORDINATE* base,
	struct BL3D_COORDINATE* world
)
{
	base->compleate_flg = FALSE;

	base->coord	= bl3d_e_matrix;
	base->workm	= bl3d_e_matrix;
	
	base->rotate	= bl3d_0_vector;
	base->transfer	= bl3d_0_vector;
	
	base->super	= world;
}

/// BL3D_COORDINATEの再帰的な更新
/// coord の compleate_flg を見て1(true)ならば、既に計算済みならば計算を省略する。
/// ただし、親側のノードのうち、いずれかが変更（flgが0）されてた場合は、通常どうり全て計算される。
/// 更新があれば戻り値は１、更新が無ければ０。
static int bl3d_recalc_coord(struct BL3D_COORDINATE* a)
{
	if(a == NULL) {
		return 0;
	}


	int recalc_flg = bl3d_recalc_coord(a->super);
	
	if(recalc_flg == TRUE || a->compleate_flg == FALSE) {
		bl3d_rot_matrix(&a->coord, &a->rotate);
		bl3d_trans_matrix(&a->coord, &a->transfer);
		
		if(a->super != NULL) {
			bl3d_comp_matrix(
				&a->workm,
				&(a->super->workm),
				&a->coord
			);
		}
		else {
			a->workm = a->coord;
		}
		
		a->compleate_flg = TRUE;
		
		return 1;
	}
	else {
		return 0;
	} 
}

/// ローカルtoワールド行列と、ローカルtoスクリーン行列を計算
/// coord の compleate_flg を見て1(true)ならば、既に計算済みならば計算を省略する。
/// ただし、親側のノードのうち、いずれかが変更（flgが0）されてた場合は、通常どうり全て計算される。
void bl3d_get_lws(
	struct BL3D_MATRIX*	lw,
	struct BL3D_MATRIX*	ls,
	struct BL3D_COORDINATE*	coord
)
{
	if(coord != NULL) {
		bl3d_recalc_coord(coord);
		*lw = coord->workm;
	}
	else {
		*lw = bl3d_e_matrix;
	}

	bl3d_comp_matrix(ls, &bl3d_ws_matrix, lw);
}

/// システムのローカルtoスクリーン行列をセットする
void bl3d_set_ls_matrix(struct BL3D_MATRIX* ls)
{
	bl3d_ls_matrix = *ls;
}

/// 光源のローカルtoワールド行列をセット
void bl3d_set_flat_light_matrix(struct BL3D_MATRIX* lw)
{
	bl3d_flat_light_matrix = *lw;
}

/// オーダリングテーブルのクリア
void bl3d_clear_ot(struct BL3D_OT* a)
{
	int i;
	for(i = 0; i < BL3D_OT_LENGTH; i++) {
		a->ot_tag_top[i]  = NULL;
		a->ot_tag_tail[i] = NULL;

#ifdef __DEBUG__
		g_printf("clear ot %d : BL3D_OT_LENGTH %d\n", i, BL3D_OT_LENGTH);
#endif // __DEBUG__
	}
}

/// ot_tagのリスト構造を辿りつつ、各ot_tag描画命令でblikeの描画バッファーに描画
/// bl3d_draw_ot()の内部処理用
static void bl3d_draw_ot_tag(struct BL3D_OT_TAG* a)
{
	if(a == NULL){
		return;
	}
	
	while(1) {
		switch(a->type) {
		case BL3D_TRIANGLE_TYPE_F:
//			bl3d_draw_triangle_f(a);
			break;
		
		case BL3D_TRIANGLE_TYPE_F_T:
//			bl3d_draw_triangle_f_t(a);
			break;
		
		case BL3D_TRIANGLE_TYPE_G:
//			bl3d_draw_triangle_g(a);
			break;
		
		case BL3D_TRIANGLE_TYPE_G_T:
			bl3d_draw_triangle_g_t(a);
			break;		
		}
	
		if(a->next == NULL) {
			break;
		}
		else {
			a = a->next;
		}
	}
}

/// オーダリングテーブルをblikeの描画バッファーに描画
void bl3d_draw_ot(struct BL3D_OT* ot)
{
	int i;
	for(i = BL3D_OT_LENGTH - 1; i >= 0; i--) {
 		bl3d_draw_ot_tag(ot->ot_tag_top[i]);
	}
}



