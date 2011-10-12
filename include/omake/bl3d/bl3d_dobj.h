#include <bl3d_types.h>
#include <bl3d_triangle.h>

#ifndef __BL3D_DOBJ_H__
#define __BL3D_DOBJ_H__

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
} __attribute__((aligned(16)));

extern void bl3d_link_object(
	struct BL3D_DOBJ* 	  	dobj,
	struct BL3D_TRIANGLE_G_T*	model_data,
	const int			model_data_len
);

extern void bl3d_sort_object(
	struct BL3D_DOBJ*	dobj,
	struct BL3D_OT*		ot
);

#endif // __BL3D_DOBJ_H__
