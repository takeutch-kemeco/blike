/* c_blike_01f_linux -
 * Copyright (C) 2011, 2012, 2013 Kemeco Takeutch <takeutchkemeco@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name of the Kemeco Takeutch nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @file drvgtk_signal.h
 * @brief blMain側スレッドと、GtkApplication側スレッドとの、スレッド間通信の仲介をする関数と構造体です。
 */

#pragma once

#include <glib.h>

/** @struct DrvGtkSignal_resize_window
 * @brief blMain側スレッドから、GtkApplicateion側スレッドへの、依頼内容（ウインドウのサイズ変更）を記述する
 */
struct DrvGtkSignal_resize_window {
        /// TRUEならシグナル依頼発行の意味。普段はFALSE。
        gboolean        ready;

        /// 希望するウインドウサイズ（よこ幅）
        gint            width;

        /// 希望するウインドウサイズ（たて幅）
        gint            height;
};

/** @struct DrvGtkSignal_show_window
 * @brief blMain側スレッドから、GtkApplicateion側スレッドへの、依頼内容（ウインドウの表示）を記述する
 */
struct DrvGtkSignal_show_window {
        /// TRUEならシグナル依頼発行の意味。普段はFALSE。
        gboolean        ready;
};

/** @struct DrvGtkSignal_flash_window
 * @brief blMain側スレッドから、GtkApplicateion側スレッドへの、依頼内容（blMain側用のフレームバッファー内容を、GtkApplication側用のフレームバッファーへ転送）を記述する
 */
struct DrvGtkSignal_flash_window {
        /// TRUEならシグナル依頼発行の意味。普段はFALSE。
        gboolean        ready;

        /// 転送元のフレームバッファーの先頭アドレス
        gpointer        src_frame_buffer;

        /// 転送を希望するフレームバッファー中の矩形領域の左上点のX座標
        gint            x;

        /// 転送を希望するフレームバッファー中の矩形領域の左上点のY座標
        gint            y;

        /// 転送を希望するフレームバッファー中の矩形領域のヨコ幅
        gint            width;

        /// 転送を希望するフレームバッファー中の矩形領域のタテ幅
        gint            height;
};

/** @struct DrvGtkSignal_exit_window
 * @brief blMain側スレッドから、GtkApplicateion側スレッドへの、依頼内容（windowの終了）を記述する
 */
struct DrvGtkSignal_exit_window {
        /// TRUEならシグナル依頼発行の意味。普段はFALSE。
        gboolean        ready;
};

/** @struct DrvGtkSignal
 * @brief struct DrvGtkSignal_??? を１つにグループ化した構造体
 * @sa gboolean update_DrvGtkSignalChain(gpointer data);
 * @brief 以下の流れとなります。
 * @par blMain側スレッドから、GtkApplication側への、スレッド間通信の仲介
 * @msc
 * a[label="blMain"], b[label="bld_???()"], x[label="struct DrvGtkSignalChain"], y[label="GtkApplication"];
 * a=>b [label="bld_???()"];
 * b=>x [label="bld_???() -> param"];
 * a<=b [label="return"];
 * x<=y [label="check (timeout)"];
 * x=>y [label="param -> message"];
 * @endmsc
 * @brief 図の左側２つ（blMain, bld_???）が、blMain側スレッド
 * @brief 図の右側(GtkApplication)が、GtkApplication側スレッド
 * @brief 図の中央(DrvGtkSignalChain構造体)が、両スレッドから読み書きできるデータ
 * @brief blMain が bld_??? 関数を実行すると、それに対応するパラメーターを DrvGtkSignalChain構造体へ書き込みます。
 * その後処理はすぐに戻ります。
 * @brief GtkApplication側スレッドは、タイムアウトループで定期的に DrvGtkSignalChain構造体の内容変化を監視しています。
 * そして内容変化あった場合は、命令（例えば画面描画など）を自身へシグナルとして送信し、実際の描画がされます。
 * @brief blMain側スレッドはコールバック関数的な状態なので、戻った時点では必ずしもbld_???の目的（例えば画面表示など）が完了してるとは限りません。
 * なので、場合によっては（画面表示完了前に、次の画像をフレームバッファへ書き込む等が起きれば）画面表示に崩れなどが生じるかもしれません。
 * @brief それを防ぐには、コールバック関数的な挙動にはせずに、スピンロックで待機（GtkApplication側スレッドでの実際の描画が完了するまで）をさせる方法が良いと思います。
 * しかし、それは現在は実装してません。
 */
struct DrvGtkSignal {
        struct DrvGtkSignal_resize_window       resize_window;
        struct DrvGtkSignal_show_window         show_window;
        struct DrvGtkSignal_flash_window        flash_window;
        struct DrvGtkSignal_exit_window         exit_window;
};

/** @fn struct DrvGtkSignal* new_DrvGtkSignal(void)
 * @brief DrvGtkSignalChain構造体をメモリーアロケート、内容を初期状態（依頼無しの状態。全てFALSE）にして先頭アドレスを返します。
 * @param 無し
 * @return (struct DrvGtkSignal*): 確保したメモリーの先頭アドレス
 */
struct DrvGtkSignal* new_DrvGtkSignal(void);

/** @fn void free_DrvGtkSignal(struct DrvGtkSignal *a)
 * @brief DrvGtkSignalChainをメモリー開放します。通常のfree()関数のラッパー関数です。
 * @param (struct DrvGtkSignal *a): 開放したい struct DrvGtkSignal構造体の先頭アドレス
 * @return 無し
 */
void free_DrvGtkSignal(struct DrvGtkSignal *a);

/** @fn gboolean update_DrvGtkSignalChain(gpointer data)
 * @brief DrvGtkSignalChain構造体の内容を監視し、必要な場合はシグナルをGtkApplicationへ送信します。
 * @param (gpointer data): 実態の型は (struct DrvGtkSignalChain *) です。これは g_timeout_add_???() に対応する型とするために gpointer として定義してます。
 * @return (gboolean): g_timeout_add_???() では、登録された関数の戻り値が TRUE なら継続、FALSE なら終了のルールとなっています。
 * g_timeout_add_???()はシステムで常に動かしたままとしたいので、ここでは常にTRUEを返します。
 * @sa struct DrvGtkSignal
 */
gboolean update_DrvGtkSignalChain(gpointer data);
