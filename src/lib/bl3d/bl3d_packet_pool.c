#include "bl3d.h"

/// 65536個までOT_TAGを提供できる。
/// つまり秒間60フレームだとすれば、秒間393万ポリゴンが理論上の限度となる。
/// …もっとも、実際はそれよりも遥に性能低いので、この数すら不可能だと思う。
/// なので現実的に十分な量のプールとして65536を選んだ。
static struct BL3D_OT_TAG pool[65536];

/// 現在のプールの最後尾
static int pool_tail = 0;

/// ot_tag として使えるメモリー領域のアドレスを借りる。
/// （注意：）このアドレスはdeleteなどの開放操作は不要。
/// bl3d_clear_ot()をトリガーとして、自動的にプールのインデックスがリセットされるので。
struct BL3D_OT_TAG* bl3d_rental_ot_tag(void)
{
	return &pool[pool_tail++];
}

/// pool をリセットし、全てのレンタルしてたパケットを無効にする。
void bl3d_reset_packet_pool(void)
{
	pool_tail = 0;
}
