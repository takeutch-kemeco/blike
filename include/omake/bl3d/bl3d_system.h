#include <bl3d_types.h>
#include <bl3d_frame_buffer.h>

#ifndef __BL3D_SYSTEM_H__
#define __BL3D_SYSTEM_H__

/// システムで使用するメインのフレームバッファー
/// このフレームバッファーに描かれた画像が、最終的にblikeのフレームバッファーへと変換・転送される。
extern struct BL3D_FRAME_BUFFER* bl3d_system_frame_buffer;



extern void bl3d_init(const int screen_width, const int screen_height);

/// 画面の中心位置のXY座標
/// これを使って、画面中心を（０、０）として考える
extern int bl3d_screen_offset[2];

/// 画面範囲
/// 描画範囲を表す、中心位置からの半径。
/// あるポリゴンを構成する頂点が、全てこの円よりも外側ならば、そのポリゴンは描画しない。
/// デフォルトではbl3d_init()時に、画面の２次元座標のノルムがセットされる。
extern float bl3d_screen_radius;

/// 描画領域を表すキューブ
/// ポリゴンの頂点が、このキューブよりも外側の場合は、キューブの最大値or最小値にアジャストする。
///
/// これにより、bl3d_screen_radiusで判定した際に、
/// １つ以上の頂点が描画範囲内の状態で、残りの頂点が描画範囲外の場合に、
/// その描画範囲頂点がスクリーンバッファー範囲の外に行ってしまうのを防止することができる。
/// 本来、これは、１ピクセル描画毎にif文で範囲判定していれば不要なのだが、
/// それだとif文の回数がピクセル回になってしまう。 一方、ポリゴンなら３回のif判定のみなので遥かに高速になる。
/// （このif文判定も、実際はsseのmaxps, minps命令を使ってるのでif判定よりもコストが少ない）
///
/// zはオーダリングテーブルの範囲。通常は BL3D_OT_LENGTH-1から bl3d_screen_projectionの範囲
/// _min _max いずれも bl3d_init()にて初期設定がセットされる。
extern struct BL3D_VECTOR bl3d_screen_cube_min;
extern struct BL3D_VECTOR bl3d_screen_cube_max;

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
extern float bl3d_ot_scale;

/// 投影面のZ座標位置の設定
/// デフォルトは1000
extern float bl3d_screen_projection;

/// 投影面の、ot配列上でのz座標の位置
/// 内部処理用
/// bl3d_screen_projection * bl3d_ot_scale である。
/// この値は bl3d_draw_ot() 毎に自動的に設定されるので、ユーザーは意識する必要は無い。
extern float bl3d_ot_projection;

/// 単位行列
extern const struct BL3D_MATRIX bl3d_e_matrix;

/// 0ベクトル
extern const struct BL3D_VECTOR  bl3d_0_vector;
extern const struct BL3D_CVECTOR bl3d_0_cvector;

/// 色を1.0形式から255形式へ変換する際に使用する定数
extern const struct BL3D_VECTOR bl3d_mul255_vector;

/// ワールドからスクリーンへの行列
extern struct BL3D_MATRIX bl3d_ws_matrix;

/// ローカルからスクリーンへの行列。
/// 主に各オブジェクトの計算時のテンポラリ変数として用いる。
extern struct BL3D_MATRIX bl3d_ls_matrix;

/// システムの環境光。
/// 光の当たってない部分が、光の当たってる部分に対してどれだけ減衰するかの割合を設定する。
/// デフォルトでは各色 1/2 となる。
extern struct BL3D_CVECTOR bl3d_ambient_depth;

/// システムの平行光源のベクトルと色。
/// 平行光源は３個まで使用できる。
extern struct BL3D_FLAT_LIGHT bl3d_system_flat_light[3];

///　システム平行光源の使用フラグ
/// 1(true)なら、そのインデックスの平行光減を、ポリゴン描画時のシェーディング計算に含める。
/// 0(false)なら含めない。
extern int bl3d_system_flat_light_use_flag[3];

/// システムの平行光源の、ローカルtoワールド行列。
/// 平行光源をワールド座標を用いて設定するのであれば、単位行列でいい。
/// ３光源で同じ座標系を共有する。
extern struct BL3D_MATRIX bl3d_flat_light_matrix;



extern void bl3d_set_view(struct BL3D_VIEW* pv);
extern void bl3d_set_flat_light(struct BL3D_FLAT_LIGHT* lt, int id);

extern void bl3d_init_coordinate(
	struct BL3D_COORDINATE* base,
	struct BL3D_COORDINATE* world
);

extern void bl3d_get_lws(
	struct BL3D_MATRIX*	lw,
	struct BL3D_MATRIX*	ls,
	struct BL3D_COORDINATE*	coord
);

extern void bl3d_get_lw(
	struct BL3D_MATRIX*	lw,
	struct BL3D_COORDINATE*	coord
);

extern void bl3d_set_ls_matrix(struct BL3D_MATRIX* ls);
extern void bl3d_set_flat_light_matrix(struct BL3D_MATRIX* lw);
extern void bl3d_clear_ot(struct BL3D_OT* ot);
extern void bl3d_draw_ot(struct BL3D_OT* ot);

#endif // __BL3D_SYSTEM_H__
