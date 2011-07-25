#include <glib.h>
#include <asoundlib.h>
#include "drvalsa_player.h"



/// drvalsaのプレイヤーでサウンドが扱える状態を表すフラグ
static gboolean drvalsa_player_ready = FALSE;



static GThread* drvalsa_player_thread;

static gboolean drvalsa_player_stop = FALSE;

/// サンプリング波形を転送パケットに送る前に、一時コピーして保管しておくためのメモリー領域
/// ダブルバッファーを行うため
static gint16* drvalsa_player_samples_bank[2][6];

/// 現在、再生に用いるバンク番号
/// これの反対が、書き込み可能なバンク番号
static gint drvalsa_player_cur_play_bank = 0;

/// 書き込みバンクが空いている状態か（バンク切り替え直後で、まだ他の書き込みがされてない状態か）
/// 書き込み可能なら TRUE
static gboolean drvalsa_player_play_bank_write_ready = TRUE;

/// バンクに新しい書き込みがあった場合TRUE
/// これを見て、現在サウンドを再生すべき状態かどうかを判断する。
/// 再生後はこのフラグをクリアし、drvalsa_cur_play_bank も反転すること！
static gboolean drvalsa_player_new_commit = FALSE;

static void drvalsa_init_player_program(void)
{
	const gint size = (drvalsa_period_size * snd_pcm_format_physical_width(drvalsa_format)) / 8;
	
	gint chn;
	for(chn = 0; chn < drvalsa_channels; chn++) {
		drvalsa_player_samples_bank[0][chn] = g_malloc(size);
		drvalsa_player_samples_bank[1][chn] = g_malloc(size);
	}
}

static void drvalsa_player_inc_cur_play_bank(void)
{
	switch(drvalsa_player_cur_play_bank) {
	case 0: drvalsa_player_cur_play_bank = 1;	break;
	case 1: drvalsa_player_cur_play_bank = 0;	break;
	}
}

static gpointer drvalsa_player_program(gpointer data)
{
	while(1) {
		if(drvalsa_player_stop == TRUE) {
			drvalsa_player_stop = FALSE;
			g_thread_exit(NULL);
		}
		
		
		if(drvalsa_player_new_commit == TRUE) {
			drvalsa_player_new_commit = FALSE;
			
			drvalsa_player_inc_cur_play_bank();
			
			drvalsa_player_play_bank_write_ready = TRUE;
			
			drvalsa_play_pcm(
				drvalsa_player_samples_bank[drvalsa_player_cur_play_bank][0],
				drvalsa_player_samples_bank[drvalsa_player_cur_play_bank][1],
				drvalsa_period_size
			);
		}
		
		
		g_usleep((1000 * 1000) / 250);		// 250Hz
	}
}

void drvalsa_create_player_thread(void)
{
	if(drvalsa_player_ready != TRUE) {
		gint err;
		
		err = drvalsa_init_player();
		if(err < 0) {
			return;
		}
		else {
			drvalsa_init_player_program();
			
			drvalsa_player_stop = FALSE;
			drvalsa_player_thread = g_thread_create(drvalsa_player_program, NULL, TRUE, NULL);
			
			drvalsa_player_ready = TRUE;
		}
	}
}

void drvalsa_close_player_thread(void)
{
	if(drvalsa_player_ready == TRUE) {
		drvalsa_player_ready = FALSE;

		drvalsa_player_stop = TRUE;
		while(drvalsa_player_stop == TRUE) {
			g_usleep((1000 * 1000) / 250);		// 250Hz
		}
		
		drvalsa_close_player();
	}
}

static gint drvalsa_player_get_cur_write_bank(void)
{
	switch(drvalsa_player_cur_play_bank) {
	case 0: return 1;
	case 1: return 0;
	}
}

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
gint drvalsa_set_pcm_player_thread(gint16* left_pcm, gint16* right_pcm)
{
	if(drvalsa_player_ready == FALSE) {
		return -1;
	}
	
	
	
	if(drvalsa_player_play_bank_write_ready == FALSE) {
		return -2;
	}
	drvalsa_player_play_bank_write_ready = FALSE;
	
	
	
	const gint bank = drvalsa_player_get_cur_write_bank();
	gint16* left_sample  = drvalsa_player_samples_bank[bank][0];
	gint16* right_sample = drvalsa_player_samples_bank[bank][1];
	
	gint i;
	for(i = 0; i < drvalsa_period_size; i++) {
		left_sample[i]  = left_pcm[i];
		right_sample[i] = right_pcm[i];
	}
	
	
	
	drvalsa_player_new_commit = TRUE;
	
	return drvalsa_period_size;
}
