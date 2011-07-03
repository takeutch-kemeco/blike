#ifndef __BL3D_H__
#define __BL3D_H__

#ifndef NULL
#define NULL  0
#endif // NULL

#ifndef TRUE
#define TRUE  1
#endif // TRUE

#ifndef FALSE
#define FALSE 0
#endif // FALSE

/// 色用ベクトル
struct BL3D_CVECTOR {
	float r, g, b, pad;
};

/// 3次元ベクトル
struct BL3D_VECTOR {
	float x, y, z, pad;
};

/// 3x3行列
///
/// 行列の引数フォーマットは m[行番号][列番号]です。（m[y座標][x座標]です）
/// 例えばm[1][2] なら
/// 0, 0, 0
/// 0, 0, ここ
/// 0, 0, 0
/// の位置を表します。
///
/// m: 3x3マトリクス係数値
/// t: 平行移動量
struct BL3D_MATRIX {
	float m[3][4];
	float t[4];
};

/// オーダリングテーブルを構成するパケット単位
///
/// 具体的には、２次元画像として描画する準備の整った３角形の、各種データが格納される。
/// （”準備の整った”とは、アフィン変換や光源・色の積和等が済んで、２D描画するのみの状態）
/// それらがポインタによる単方向リスト構造として数珠つなぎとなって、実際に運用される。
///
/// next: 次のリストへのポインタ。NULLの場合はリストの最後という意味。
/// vertex[3]: ３角形の頂点座標0,1,2
/// texture[3]: vertexに対応した頂点のテクスチャー座標0,1,2
/// color[3]: vertexに対応した頂点の色値
/// 
struct BL3D_OT_TAG {
	int			type;
	int			texture_vram;
	struct BL3D_OT_TAG*	next;
	struct BL3D_VECTOR	vertex[3];
	struct BL3D_VECTOR	texture[3];
	struct BL3D_CVECTOR	color[3];
};

/// オーダリングテーブル
///
/// 「あとは２次元画像として描画するだけ」という状態の各３角形を、線形リストにソートして保持する。
/// 具体的には BL3D_OT_TAG型のポインタの配列があり、これに実際にリストとして登録していく。
/// このポインタ配列のインデックス値はZ座標に対応し、インデックスの大きい側から順に、
/// 登録されている三角形が描画される。
/// つまり、Z座標の遠いものから描画されて、Z座標の近いもで上書きされることで、
/// Zソートによる描画を行うためのもの。
///
/// この構造体にデータをセットし、それを bl3d_draw_ot() することで、
/// blike の描画バッファーに描画される。
/// （注意：blikeの描画バッファーに描画しただけでは画面表示されない。
///  wait()を実行して実際にフラッシュされるまで、実際のウインドウ画面には絵が出ないので注意）
///
/// ot_tag_top[]:BL3D_OT_TAGへのポインタの配列。
///	配列のサイズは BL3D_OT_LENGTH で決め打ちである。（現状では0 ~ 32767）
///	ここがNULLの場合は、そのZには描画すべき３角形が無いということ。
///	bl3d_clear_ot() によって、全てNULLがセットされる。
/// ot_tag_tail: 各インデックスでの ot_tag リストの最後尾のアドレス。
///	ot_tag がNULLの場合はNULL。
///	ot_tag にリストを追加した場合は、責任をもってこのアドレスも変更すること。
#define BL3D_OT_LENGTH (1<<15)
struct BL3D_OT {
	struct BL3D_OT_TAG*	ot_tag_top[BL3D_OT_LENGTH];
	struct BL3D_OT_TAG*	ot_tag_tail[BL3D_OT_LENGTH];
};

/// 行列型の座標系
/// compleate_flg: workmの値が計算済みの場合は1(true)がセットされる。
///	これによって、すでに計算済みのworkmの再計算を省略することができる。
///	(注意！）ローカル座標の値を変更した場合は、責任をもってflgを0(false)にする必要がある。
///	さもなければ、値をセットしても計算されないことになる。
/// coord: ローカル座標系での、回転・移動の行列
/// rotate: ローカル座標系での回転係数。この値から自動的にcoordが計算される
/// transfer: ローカル座標系での平行移動係数。この値から自動的にcoordが計算される
/// workm: この座標系からワールド座標系までの積
/// super: 親座標系へのポインタ。０の場合は自身がワールド座標系という意味。
struct BL3D_COORDINATE {
	int			compleate_flg;
	struct BL3D_MATRIX	coord;
	struct BL3D_VECTOR	rotate;
	struct BL3D_VECTOR	transfer;
	struct BL3D_MATRIX	workm;
	struct BL3D_COORDINATE*	super;	
};

/// 視点位置
/// view: ワールド座標から視点座標への変換行列
/// super: 視点を設定する座標系へのポインタ
struct BL3D_VIEW {
	struct BL3D_MATRIX	view;
	struct BL3D_COORDINATE*	super;
};

/// 平行光源
/// これを bl3d_set_flat_light()によってシステムにセットする。
/// システムには３個まで光源を設定することができる。
/// vector: ワールド座標上での、光源の方向ベクトル
/// color: 光の色
struct BL3D_FLAT_LIGHT {
	struct BL3D_VECTOR	vector;
	struct BL3D_CVECTOR	color;
};






/// bl3d_math.c

/// sin
extern float bl3d_sin(float a);

/// cos
extern float bl3d_cos(float a);

/// sqrt
extern float bl3d_sqrt(float a);






/// bl3d_matrix.c

/// ２つの行列の積をとります。
/// 計算結果はdstに格納されます。
/// 計算順序は src0 * src1 = dst です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
extern struct BL3D_MATRIX* bl3d_mul_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src0,
	struct BL3D_MATRIX* src1
);

/// ベクトルに行列を乗算します。
/// 計算結果はdstに格納されます。
/// 計算順序は m * v = dst です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
extern struct BL3D_VECTOR* bl3d_apply_matrix(
	struct BL3D_VECTOR* dst,
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* v
);

/// x軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
extern struct BL3D_MATRIX* bl3d_rot_matrix_x(
	struct BL3D_MATRIX* 	m,
	float			r
);

/// y軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
extern struct BL3D_MATRIX* bl3d_rot_matrix_y(
	struct BL3D_MATRIX* 	m,
	float			r
);

/// z軸の回転角から回転行列を求める。
/// 計算結果は m に格納されます。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
extern struct BL3D_MATRIX* bl3d_rot_matrix_z(
	struct BL3D_MATRIX* 	m,
	float			r
);

/// 回転角から回転行列を求める。
/// 計算結果は m に格納されます。
/// 回転行列の乗算順序は z * y * x = m です。
///
/// （注意：計算するのは回転行列m[][]のみで、平行移動t[]は計算しません）
extern struct BL3D_MATRIX* bl3d_rot_matrix(
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* r
);

/// 平行移動量を行列にセットする。
/// 計算結果は m に格納されます。
extern struct BL3D_MATRIX* bl3d_trans_matrix(
	struct BL3D_MATRIX* m,
	struct BL3D_VECTOR* v
);

/// 座標変換の合成を行います。
/// bl3d_mul_matrix()とは異なり、平行移動も含めて合成します。
/// 計算結果はdstに格納されます。
///
/// 行列の計算順序は src0.m * src1.m = dst.m です。
/// 平行移動の計算順序は (src0.m * src1.t) + src0.t = dst.t です。
/// つまり src0 が親側で、src1が子側です。
extern struct BL3D_MATRIX* bl3d_comp_matrix(
	struct BL3D_MATRIX* dst,
	struct BL3D_MATRIX* src0,
	struct BL3D_MATRIX* src1
);

///　コンソールへの値出力
/// デバッグ用
extern void bl3d_print_matrix(struct BL3D_MATRIX* a);






/// bl3d_system.c

/// 画面の中心位置のXY座標
/// これを使って、画面中心を（０、０）として考える
extern int bl3d_screen_offset[2];

/// 投影面のZ座標位置の設定
/// デフォルトは1000
extern int bl3d_screen_projection;

/// 単位行列
extern const struct BL3D_MATRIX bl3d_e_matrix;

/// 0ベクトル
extern const struct BL3D_VECTOR bl3d_0_vector;

/// ワールドからスクリーンへの行列
extern struct BL3D_MATRIX bl3d_ws_matrix;

/// ローカルからスクリーンへの行列。
/// 主に各オブジェクトの計算時のテンポラリ変数として用いる。
extern struct BL3D_MATRIX bl3d_ls_matrix;

/// システムの環境光。
/// 光の当たってない部分が、光の当たってる部分に対してどれだけ減衰するかの割合を設定する。
/// デフォルトでは各色 1/4 となる。
extern struct BL3D_CVECTOR bl3d_ambient_depth;

/// システムの平行光源のベクトルと色。
/// 平行光源は３個まで使用できる。
extern struct BL3D_VECTOR bl3d_flat_light_vector[3];
extern struct BL3D_CVECTOR bl3d_flat_light_color[3];

/// システムの平行光源の、ローカルtoワールド行列。
/// 平行光源をワールド座標を用いて設定するのであれば、単位行列でいい。
/// ３光源で同じ座標系を共有する。
extern struct BL3D_MATRIX bl3d_flat_light_matrix;



/// システムのワールドtoスクリーン行列を設定する。
/// BL3D_VIEWの親座標をワールド以外に設定した場合は、
/// 親座標系の値の変化した場合、視点も影響を受けるため、この関数を毎フレーム実行する必要がある。
extern void bl3d_set_view(struct BL3D_VIEW* pv);

/// システムの平行光源を設定する。
/// 平行光源は３個まで使用できる。
///
/// lt: 光源情報
/// id: 光源番号(0 ~ 2)
extern void bl3d_set_flat_light(struct BL3D_FLAT_LIGHT* lt, int id);

/// ローカル座標系の初期化
/// base: 初期化する座標系へのポインタ
/// super: 親座標系へのポインタ。NULLの場合はbaseをワールド座標系と考える。
extern void bl3d_init_coordinate(
	struct BL3D_COORDINATE* base,
	struct BL3D_COORDINATE* world
);

/// ローカルtoワールド行列と、ローカルtoスクリーン行列を計算
/// coord の compleate_flg を見て1(true)ならば、既に計算済みならば計算を省略する。
/// ただし、親側のノードのうち、いずれかが変更（flgが0）されてた場合は、通常どうり全て計算される。
extern void bl3d_get_lws(
	struct BL3D_MATRIX*	lw,
	struct BL3D_MATRIX*	ls,
	struct BL3D_COORDINATE*	coord
);

/// システムのローカルtoスクリーン行列をセットする
extern void bl3d_set_ls_matrix(struct BL3D_MATRIX* ls);

/// 光源のローカルtoワールド行列をセット
extern void bl3d_set_flat_light_matrix(struct BL3D_MATRIX* lw);

/// オーダリングテーブルのクリア
extern void bl3d_clear_ot(struct BL3D_OT* ot);

/// オーダリングテーブルをblikeの描画バッファーに描画
extern void bl3d_draw_ot(struct BL3D_OT* ot);






/// bl3d_triangle.c

/// BL3Dで描画できる三角形の、各種タイプ
#define BL3D_TRIANGLE_TYPE_F	(0+0)	/// フラットシェーディング、テクスチャー無し
#define BL3D_TRIANGLE_TYPE_F_T	(0+16)	/// フラットシェーディング、テクスチャー有り
#define BL3D_TRIANGLE_TYPE_G	(8+0)	/// グラデーションシェーディング、テクスチャー無し
#define BL3D_TRIANGLE_TYPE_G_T 	(8+16)	/// グラデーションシェーディング、テクスチャー有り

struct BL3D_TRIANGLE_G_T {
 	int			type;
	int			texture_vram;
	struct BL3D_VECTOR	vertex[3];
	struct BL3D_VECTOR	texture[3];
	struct BL3D_CVECTOR	color[3];
	struct BL3D_OT_TAG	ot_tag;
	
};

extern void bl3d_init_triangle_g_t(
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
);

extern void bl3d_sort_triangle_g_t(
	struct BL3D_TRIANGLE_G_T*	a,
	struct BL3D_OT*			ot
);

extern void bl3d_draw_triangle_g_t(struct BL3D_OT_TAG* a);






/// bl3d_dobj.c

/// ３次元オブジェクトハンドラ
/// この構造体によって、ポリゴンによって構成された一塊のオブジェクトを扱う。
/// local_coord: このオブジェクト自身のローカル座標系。
/// model_data: BL3D_TRIANGLE_G_T 型の配列の先頭アドレス
///	すべてのポリゴンをBL3D_TRIANGLE_G_T（テクスチャー・グローシェーディング）として扱う。
///	理由は、モデリングデータの構造を単純にしたかったので。
///	たとえばノンテクスチャー・フラットの場合も、テクスチャー・グローの特殊な形態の一つと考える。
/// model_data_len: 配列の長さ。　つまり、このオブジェクトが扱うポリゴン数。
struct BL3D_DOBJ {
	struct BL3D_COORDINATE 		local_coord;
	struct BL3D_TRIANGLE_G_T*	model_data;
	int				model_data_len;
};



/// ３次元オブジェクトハンドラの初期設定。
extern void bl3d_link_object(
	struct BL3D_DOBJ* 	  	dobj,
	struct BL3D_TRIANGLE_G_T*	model_data,
	int				model_data_len
);

/// オーダリングテーブルにオブジェクトを割り付ける。
/// この関数によって、dobjの各ポリゴンにジオメトリー演算が行われ、
/// オーダリングテーブルに各ポリゴンの最終的な2次元描画パケットを登録する。
extern void bl3d_sort_object(
	struct BL3D_DOBJ*	dobj,
	struct BL3D_OT*		ot
);



#endif // __BL3D_H__
