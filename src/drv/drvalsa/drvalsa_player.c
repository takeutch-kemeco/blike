#include <glib.h>
#include <asoundlib.h>



/// デバイス名はdefaultを指定する。
/// ここは大抵alsaのミキサーが割り当てられてるだろうから。
/// もしもplughw:0,0などとして直接指定してしまうと、flash等の他アプリとデバイスの奪い合いが発生してしまうことがある。
static const gchar* drvalsa_device = "default";

/// 普通は stdout を登録して、ダンプリストなどがコンソールに出力されるようにしたりするためのもの。
static snd_output_t* drvalsa_output = NULL;



/// バッファー全体のサイズ
static snd_pcm_sframes_t drvalsa_buffer_size;

/// バッファー単位（１回辺りの、サウンドカードに渡す量。普通はサウンドカードのバッファーサイズというと、これのこと）
snd_pcm_sframes_t drvalsa_period_size;

/// バッファー全体と、バッファー単位のサイズ
/// これはバイト単位ではなくマイクロ秒単位。つまり、実際の必要バッファーサイズは、この時間*サンプリング深度*チャンネル数となる
static guint drvalsa_buffer_time = 500000;
static guint drvalsa_period_time = 100000;



/// スピーカーチャンネル数
/// 無難にステレオをデフォルト仕様にするのがいいと思う。（今時ステレオ再生できないパソコンなんて無いだろう）
const guint drvalsa_channels = 2;

/// wav のサンプリング周波数を再生時に変更可能にする
static const gint drvalsa_resample = 1;

/// 音声PCMデータの形式は、ウィンドウズの 16bit44100KHz の符号あり wav 形式を想定している。
/// （音声データが符号ありの16bit、つまり signed short で記録されたものを想定。 ウインドウズの wav は確かこの形式）
const snd_pcm_format_t	drvalsa_format = SND_PCM_FORMAT_S16;
static const guint 	drvalsa_rate   = 44100;

/// period を行うかどうかのフラグ
static const gint drvalsa_period_event = 0;

/// ストリームのリカバリーを行うかどうかのフラグ
static const gint drvalsa_verbose = 0;






/// サウンドデバイスのハンドル
static snd_pcm_t* drvalsa_handle;

/// ハード側、ソフト側、それぞれの設定パラメーターのコンテナ
static snd_pcm_hw_params_t* drvalsa_hw_params;
static snd_pcm_sw_params_t* drvalsa_sw_params;

/// サンプリング波形の転送パケットに用いる、実際のメモリー領域
static gint16* drvalsa_samples;

/// 転送バッファー内にデータを書き込む際のパーテーションの参考
static snd_pcm_channel_area_t* drvalsa_areas;






static gint drvalsa_set_hw_params(
	snd_pcm_t* 		handle,
	snd_pcm_hw_params_t*	params,
	snd_pcm_access_t	access
)
{
	guint rrate;
	snd_pcm_uframes_t size;
	gint err;
	gint dir;
	
	// とりあえず、ハードウェアのパラメーターをもろもろparamsに読み込む
	err = snd_pcm_hw_params_any(handle, params);
	if (err < 0) {
		return err;
	}
	
	// リサンプリングするフラグをセットする
	err = snd_pcm_hw_params_set_rate_resample(handle, params, drvalsa_resample);
	if (err < 0) {
		return err;
	}
	
	//read,writeのインターリーブ設定のセット
	err = snd_pcm_hw_params_set_access(handle, params, access);
	if (err < 0) {
		return err;
	}

	// 符号あり16bitPCMとして再生するという設定をセット
	err = snd_pcm_hw_params_set_format(handle, params, drvalsa_format);
	if (err < 0) {
		return err;
	}

	// サウンド再生のチャンネル数をセット
	err = snd_pcm_hw_params_set_channels(handle, params, drvalsa_channels);
	if (err < 0) {
		return err;
	}
	
	// 44100Hzとして再生
	rrate = drvalsa_rate;
	err = snd_pcm_hw_params_set_rate_near(handle, params, &rrate, 0);
	if (err < 0) {
		return err;
	}
	// セットした値がセットできなかった場合は、最初にセットした値と相違がでる。そこでエラーを判定
	if (rrate != drvalsa_rate) {
		return -EINVAL;
	}
	
	// バッファーのサイズを、まず最初に、”時間単位”で指定する
	// で、get_drvalsa_buffer_size()で、バッファーが設定できたかを調べてる
	// そのバッファーサイズは、他の関数で使うようにグローバルに保存
	err = snd_pcm_hw_params_set_buffer_time_near(handle, params, &drvalsa_buffer_time, &dir);
	if (err < 0) {
		return err;
	}
	err = snd_pcm_hw_params_get_buffer_size(params, &size);
	if (err < 0) {
		return err;
	}
	drvalsa_buffer_size = size;

	// バッファーサイズ同様。
	err = snd_pcm_hw_params_set_period_time_near(handle, params, &drvalsa_period_time, &dir);
	if (err < 0) {
		return err;
	}
	err = snd_pcm_hw_params_get_period_size(params, &size, &dir);
	if (err < 0) {
		return err;
	}
	drvalsa_period_size = size;

	
	// params パラメーターをデバイス設定にセットする
	err = snd_pcm_hw_params(handle, params);
	if (err < 0) {
		return err;
	}
	
	return 0;
}

static int drvalsa_set_sw_params(snd_pcm_t* handle, snd_pcm_sw_params_t* swparams)
{
	gint err;
	
	err = snd_pcm_sw_params_current(handle, swparams);
	if (err < 0) {
		return err;
	}
	
	err = snd_pcm_sw_params_set_start_threshold(handle, swparams, (drvalsa_buffer_size / drvalsa_period_size) * drvalsa_period_size);
	if (err < 0) {
		return err;
	}
	
	err = snd_pcm_sw_params_set_avail_min(handle, swparams, drvalsa_period_event ? drvalsa_buffer_size : drvalsa_period_size);
	if (err < 0) {
		return err;
	}
	
	if (drvalsa_period_event != 0) {
		err = snd_pcm_sw_params_set_period_event(handle, swparams, 1);
		if (err < 0) {
			return err;
		}
	}

	err = snd_pcm_sw_params(handle, swparams);
	if (err < 0) {
		return err;
	}

	return 0;
}

gint drvalsa_init_player(void)
{
	gint err;

	snd_pcm_hw_params_malloc(&drvalsa_hw_params);
	snd_pcm_sw_params_malloc(&drvalsa_sw_params);
	
	
	
	// ダンプなどの出力先をstdoutにする
 	err = snd_output_stdio_attach(&drvalsa_output, stdout, 0);
	if(err < 0) {
		return err;
	}
	
	err = snd_pcm_open(&drvalsa_handle, drvalsa_device, SND_PCM_STREAM_PLAYBACK, 0);
	if(err < 0) {
		return err;
	}
	
	err = drvalsa_set_hw_params(drvalsa_handle, drvalsa_hw_params, SND_PCM_ACCESS_RW_INTERLEAVED);
	if(err < 0) {
		return err;
	}
	
	err = drvalsa_set_sw_params(drvalsa_handle, drvalsa_sw_params);
	if(err < 0) {
		return err;
	}
	
	drvalsa_samples = g_malloc((drvalsa_period_size * drvalsa_channels * snd_pcm_format_physical_width(drvalsa_format)) / 8);
	if (drvalsa_samples == NULL) {
		return err;
	}
	
	drvalsa_areas = g_malloc(drvalsa_channels * sizeof(snd_pcm_channel_area_t));
	if (drvalsa_areas == NULL) {
		return err;
	}
	
	guint chn;
	for (chn = 0; chn < drvalsa_channels; chn++) {
		drvalsa_areas[chn].addr = drvalsa_samples;
		drvalsa_areas[chn].first = chn * snd_pcm_format_physical_width(drvalsa_format);
		drvalsa_areas[chn].step = drvalsa_channels * snd_pcm_format_physical_width(drvalsa_format);
	}
	
	return 0;
}

/// drvalsa_make_packet()の内部関数
/// pcmからパケットの任意チャンネル番用のバファーへ、count分だけコピー
static void drvalsa_copy_sample_to_packet(
	gint16*		pcm,
	gint		count,
	const guint	chn
)
{
	if(chn >= drvalsa_channels) {
		return;
	}
	
	
	
	guchar* samples = ((guchar*)drvalsa_areas[chn].addr) + (drvalsa_areas[chn].first / 8);
	gint    steps   = drvalsa_areas[chn].step / 8;
	
	const gint bps = snd_pcm_format_width(drvalsa_format) / 8;	// bytes per sample
	
	while(count-- > 0) {
		gint i;
		for (i = 0; i < bps; i++) {
			*(samples + i) = ((*pcm) >>  (i * 8)) & 0xff;
		}
		
		samples += steps;
		pcm++;
	}
}

/// drvalsa_make_packet()の内部関数
/// 0をパケットの任意チャンネル番用のバファーのoffset番目から、count分だけコピー
static void drvalsa_copy_0_to_packet(
	const gint	offset,
	gint		count,
	const guint	chn
)
{
	if(chn >= drvalsa_channels) {
		return;
	}
	
	
	
	guchar* samples = ((guchar*)drvalsa_areas[chn].addr) + (drvalsa_areas[chn].first / 8) + offset;
	gint    steps   = drvalsa_areas[chn].step / 8;
	
	const gint bps = snd_pcm_format_width(drvalsa_format) / 8;	// bytes per sample
	
	while(count-- > 0) {
		gint i;
		for (i = 0; i < bps; i++) {
			*(samples + i) = 0;
		}
		
		samples += steps;
	}
}

/// サウンドデバイスに渡すパケットを生成する。
/// 生成されるパケットのサイズはシステムで固定である。
///
/// 渡された pcm の pcm_index 番目からのパケットサイズ分が、pcm_lenをオーバーした場合は、
/// 残りのパケット領域は無音（０）で埋められる。
///
/// pcm: signed short型の配列の先頭アドレス。
///	これは、符号あり16bit, 44100Hz、リトルエンディアン配置、モノラル音声、の波形データである。
///	（ウィンドウズのwav形式の波形部分のデータに相当）
/// 
/// pcm_len: signed short型単位での pcm 全体の長さ。 （バッファーオーバーリードしてしまわないように使う）
///
/// chn: pcm再生に用いるチャンネル番号(ステレオならば 0 or 1)
///
static void drvalsa_make_packet(
	gint16*		pcm,
	const gint	pcm_len,
	const gint	chn
)
{
	if(drvalsa_period_size > pcm_len) {
		drvalsa_copy_sample_to_packet(pcm, pcm_len, chn);
		
		drvalsa_copy_0_to_packet(
			pcm_len,
			drvalsa_period_size - pcm_len,
			chn
		);
	}
	else {
		drvalsa_copy_sample_to_packet(pcm, drvalsa_period_size, chn);
	}
}

void drvalsa_play_pcm(gint16* left_pcm, gint16* right_pcm, gint pcm_len)
{
	signed short *ptr;
	int err, cptr;
	
	gint pcm_count = pcm_len;
	
	while(pcm_count > 0) {
		drvalsa_make_packet(left_pcm,  pcm_len, 1);
		drvalsa_make_packet(right_pcm, pcm_len, 0);
		
		pcm_len   -= drvalsa_period_size;
		left_pcm  += drvalsa_period_size;
		right_pcm += drvalsa_period_size;
		
		
		snd_pcm_writei(drvalsa_handle, drvalsa_samples, drvalsa_period_size);
		
		
		pcm_count -= drvalsa_period_size;
	}
}

void drvalsa_close_player(void)
{
	free(drvalsa_areas);
	free(drvalsa_samples);
	snd_pcm_close(drvalsa_handle);
}
