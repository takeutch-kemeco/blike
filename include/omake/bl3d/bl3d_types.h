#ifndef __BL3D_TYPES_H__
#define __BL3D_TYPES_H__

/// SSE3を使用する。
/// もしSSE3を使用しないライブラリとしてコンパイルしたい場合は
/// この宣言をコメントアウトする。
#define __ENABLE_SSE3__

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
	float b, g, r, pad;
} __attribute__((aligned(16)));

/// 3次元ベクトル
struct BL3D_VECTOR {
	float x, y, z, pad;
} __attribute__((aligned(16)));

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
} __attribute__((aligned(16)));

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
/// base_color: ３つの平行光源を合成した結果。
///	この値は色値の0位置を、どこにするかという下駄を設定する。
///	たとえばこれが50%ならば、128なので、128を0として、下駄を履かせて色を計算することになる。
///	結果、255を飛び出した分は、飽和色として、255に丸める。
struct BL3D_OT_TAG {
	int			type;
	int			texture_vram;
	struct BL3D_OT_TAG*	next;
	struct BL3D_VECTOR	vertex[3];
	struct BL3D_VECTOR	texture[3];
	struct BL3D_CVECTOR	color[3];
	struct BL3D_CVECTOR	base_color;
} __attribute__((aligned(16)));

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
///	配列のサイズは BL3D_OT_LENGTH で決め打ちである。（現状では0 ~ 16535）
///	ここがNULLの場合は、そのZには描画すべき３角形が無いということ。
///	bl3d_clear_ot() によって、全てNULLがセットされる。
/// ot_tag_tail: 各インデックスでの ot_tag リストの最後尾のアドレス。
///	ot_tag がNULLの場合はNULL。
///	ot_tag にリストを追加した場合は、責任をもってこのアドレスも変更すること。
#define BL3D_OT_LENGTH (1<<14)
struct BL3D_OT {
	struct BL3D_OT_TAG*	ot_tag_top[BL3D_OT_LENGTH];
	struct BL3D_OT_TAG*	ot_tag_tail[BL3D_OT_LENGTH];
} __attribute__((aligned(16)));

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
} __attribute__((aligned(16)));

/// 視点位置
/// view: ワールド座標から視点座標への変換行列
/// super: 視点を設定する座標系へのポインタ
struct BL3D_VIEW {
	struct BL3D_MATRIX	view;
	struct BL3D_VECTOR	rotate;
	struct BL3D_VECTOR	transfer;
	struct BL3D_COORDINATE	local_coord;
} __attribute__((aligned(16)));

/// 平行光源
/// これを bl3d_set_flat_light()によってシステムにセットする。
/// システムには３個まで光源を設定することができる。
/// vector: ワールド座標上での、光源の方向ベクトル
/// color: 光の色
struct BL3D_FLAT_LIGHT {
	struct BL3D_VECTOR	vector;
	struct BL3D_CVECTOR	color;
} __attribute__((aligned(16)));

#endif // __BL3D_TYPES_H__
