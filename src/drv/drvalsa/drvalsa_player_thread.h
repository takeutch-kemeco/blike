#include <glib.h>

#ifndef __DRVALSA_PLAYER_THREAD_H__
#define __DRVALSA_PLAYER_THREAD_H__

#include <glib.h>
#include "drvalsa_player.h"

extern void drvalsa_create_player_thread(void);
extern void drvalsa_close_player_thread(void);

/// サウンドプレイヤーのスレッドに、波形サンプルのパケットをセットする。（セットは、実際にはコピーによって行われる）
/// セットされた波形サンプルは、スレッドによってプレイヤーに渡されて、プレイヤーは適切なタイミングでサウンドカードへ波形サンプルを渡す。
/// これによりサウンドが再生される。
/// 
/// 戻り値は、０未満の場合は、セットできなかったことを示。
/// 	−１の場合は、サウンドのプレイヤーのシステムそのものが使用できない状態。
/// 	（たとえばサウンドボードが物理的に無いなどの理由で初期化に失敗したなど）
///	-2の場合は、すでにサウンドスレッドのバッファーに波形サンプルがセットされてる状態で、それがまだプレイヤーへ転送されてない状態。
///	
///	戻り値が０以上の場合は、バッファーにセットされた分のサンプルの長さを表す。
///	この戻り値を頼りに、呼び出し側はサンプルのシーク位置を次にすすめることができる。（実は、この値はシステムで固定なので、定数でもいいのだが）
extern gint drvalsa_set_pcm_player_thread(gint16* left_pcm, gint16* right_pcm);

#endif // __DRVALSA_PLAYER_THREAD_H__
