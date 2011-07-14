//#define __DEBUG__

#include "bl3d.h"

/// 画面の中心位置のXY座標
/// これを使って、画面中心を（０、０）として考える
int bl3d_screen_offset[2];

/// 画面範囲
/// 描画範囲を表す、中心位置からの半径。
/// あるポリゴンを構成する頂点が、全てこの円よりも外側ならば、そのポリゴンは描画しない。
/// デフォルトではbl3d_init()時に、画面の２次元座標のノルムがセットされる。
float bl3d_screen_radius;

/// オーダリングテーブルへの、Z割り当て時の、ｚスケーリング係数
/// 
/// オーダリングテーブルによるｚソートは、
/// 配列のインデックスを頼りにしてポリゴンの前後判定を行っているので、
/// あまり極端にｚの精度を高めようとして BL3D_OT_LENGTH を大きな数にしてしまうと、
/// 毎フレームのｚソートの検索処理において、その処理速度に、無視できない影響を与えてしまう。
///
/// そのため、オーダリングテーブルのｚは、あまり大きな値にはできない。
/// しかし、それだと、視点からの距離を表現するにおいて、
/// ｚをとても大きな値にしたい時に、できなくてこまる。
///
/// そこで、オーダリングテーブルへのｚ割り当て時に、ｚを適当にスケーリングして
/// 割り当てるという仕組みを導入する。
/// その際のスケーリング係数として、この変数を用いる。
/// 
/// たとえば、BL3d_OT_LENGTH が 10000 だったとして、10000よりも大きなZを扱いたい場合
/// たとえば z を 0 ~ 50000 の範囲で扱いたい場合であれば、
/// この変数を BL3D_OT_LENGTH * (10000 / 50000) とすれば、
/// ポリゴンのotソートの関数を用いた際に、
/// z = 50000のポリゴンなら、ot_tag[10000]の位置に割り当てられるようになる。
///
/// 注意：
/// この値を小さくすればするほど、遠くのｚまで描画できるようになるが、
/// その代償として、ポリゴンの前後判定の精度が落ちる。
/// （同様のot_tag[?]に割り当てられることで、前後関係が無視されて、
/// 　ｚの前後関係が逆になる頻度が高まるので）
/// 古いゲームなどでよく見られる、ポリゴンがパカパカする現象は、
/// これの値を欲張りすぎたために起こる現象。
///
/// デフォルトは1/4倍。
float bl3d_ot_scale = 1.0 / 4.0;

/// 投影面のZ座標位置の設定
/// デフォルトは1000
float bl3d_screen_projection = 1000;

/// 投影面の、ot配列上でのz座標の位置
/// 内部処理用
/// bl3d_screen_projection * bl3d_ot_scale である。
/// この値は bl3d_draw_ot() 毎に自動的に設定されるので、ユーザーは意識する必要は無い。
float bl3d_ot_projection = 1000 / 4.0;

/// 単位行列
const struct BL3D_MATRIX bl3d_e_matrix = {
	.m[0][0] = 1, .m[0][1] = 0, .m[0][2] = 0,
	.m[1][0] = 0, .m[1][1] = 1, .m[1][2] = 0,
	.m[2][0] = 0, .m[2][1] = 0, .m[2][2] = 1,
	.t[0] = 0, .t[1] = 0, .t[2] = 0
};

/// 0ベクトル
const struct BL3D_VECTOR  bl3d_0_vector  = {.x = 0, .y = 0, .z = 0, .pad = 0};
const struct BL3D_CVECTOR bl3d_0_cvector = {.r = 0, .g = 0, .b = 0, .pad = 0};

/// 色を1.0形式から255形式へ変換する際に使用する定数
const struct BL3D_VECTOR bl3d_mul255_vector = {255, 255, 255, 0};

/// ワールドからスクリーンへの行列
struct BL3D_MATRIX bl3d_ws_matrix;

/// ローカルからスクリーンへの行列。
/// 主に各オブジェクトの計算時のテンポラリ変数として用いる。
struct BL3D_MATRIX bl3d_ls_matrix;

/// システムの環境光。
/// 光の当たってない部分が、光の当たってる部分に対してどれだけ減衰するかの割合を設定する。
/// デフォルトでは各色 1/2 となる。
struct BL3D_CVECTOR bl3d_ambient_depth = {
	.r = 1.0/2, .g = 1.0/2, .b = 1.0/2
};

/// システムの平行光源のベクトルと色。
/// 平行光源は３個まで使用できる。
struct BL3D_FLAT_LIGHT bl3d_system_flat_light[3] = {
	{
		.vector.x = 0,	.vector.y = 0,	.vector.z = 1.0,
		.color.r =1.0,	.color.g =1.0,	.color.b =1.0
	},
	{
		.vector.x = 0,	.vector.y = 1.0,.vector.z = 0,
		.color.r =1.0,	.color.g =1.0,	.color.b =1.0
	},
	{
		.vector.x = 1.0,.vector.y = 0,	.vector.z = 0,
		.color.r =1.0,	.color.g =1.0,	.color.b =1.0
	}
};

///　システム平行光源の使用フラグ
/// 1(true)なら、そのインデックスの平行光減を、ポリゴン描画時のシェーディング計算に含める。
/// 0(false)なら含めない。
int bl3d_system_flat_light_use_flag[3] = {0, 0, 0};

/// システムの平行光源の、ローカルtoワールド行列。
/// 平行光源をワールド座標を用いて設定するのであれば、単位行列でいい。
/// ３光源で同じ座標系を共有する。
struct BL3D_MATRIX bl3d_flat_light_matrix;

/// システムの初期設定
/// bl3dで３次元表示を行う場合は、最初にこれを実行しておくのが望ましい。
/// 画像表示のオフセットや、各種変換行列の初期化などを行う。
///
/// screen_width:スクリーンの横幅（ピクセル単位）
/// screen_height:スクリーンの縦幅（ピクセル単位）
void bl3d_init(const int screen_width, const int screen_height)
{
	bl3d_screen_offset[0] = screen_width / 2;
	bl3d_screen_offset[1] = screen_height / 2;
	
	struct BL3D_VECTOR scr = {
		.x =screen_width / 2.0, .y = screen_height / 2.0, .z = 0.0
	};
	bl3d_screen_radius = bl3d_norm_vector(&scr);
	
	bl3d_ws_matrix = bl3d_e_matrix;
	bl3d_ls_matrix = bl3d_e_matrix;
}

/// システムのワールドtoスクリーン行列を設定する。
/// BL3D_VIEWの親座標をワールド以外に設定した場合は、
/// 親座標系の値の変化した場合、視点も影響を受けるため、この関数を毎フレーム実行する必要がある。
void bl3d_set_view(struct BL3D_VIEW* pv)
{
	struct BL3D_MATRIX lw;
	bl3d_get_lw(&lw, &pv->local_coord);
	
	struct BL3D_MATRIX im;
	bl3d_rot_matrix(&im, &pv->rotate);
	bl3d_invert_matrix(&pv->view, &im);
		
	pv->view.t[0] = -pv->transfer.x;
	pv->view.t[1] = -pv->transfer.y;
	pv->view.t[2] = -pv->transfer.z;
	
	bl3d_comp_matrix(&bl3d_ws_matrix, &pv->view, &lw);
}

/// システムの平行光源を設定する。
/// 平行光源は３個まで使用できる。
///
/// lt: 光源情報
/// id: 光源番号(0 ~ 2)
void bl3d_set_flat_light(struct BL3D_FLAT_LIGHT* lt, int id)
{
	id &= 0x03;
	
	
	bl3d_system_flat_light[id].color = lt->color;
	
	
	struct BL3D_VECTOR* p = &bl3d_system_flat_light[id].vector;
	bl3d_apply_matrix(p, &bl3d_ws_matrix, &lt->vector);
	bl3d_unit_vector(p, p);
	
	bl3d_system_flat_light_use_flag[id] = TRUE;
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

/// ローカルtoワールド行列を計算
/// coord の compleate_flg を見て1(true)ならば、既に計算済みならば計算を省略する。
/// ただし、親側のノードのうち、いずれかが変更（flgが0）されてた場合は、通常どうり全て計算される。
void bl3d_get_lw(
	struct BL3D_MATRIX*	lw,
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
	
	bl3d_reset_packet_pool();
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
	
	
	bl3d_ot_projection = bl3d_screen_projection * bl3d_ot_scale;
}

/// オーダリングテーブルをblikeの描画バッファーに描画
void bl3d_draw_ot(struct BL3D_OT* ot)
{
	int i;
	for(i = BL3D_OT_LENGTH - 1; i >= 0; i--) {
 		bl3d_draw_ot_tag(ot->ot_tag_top[i]);
	}
}



