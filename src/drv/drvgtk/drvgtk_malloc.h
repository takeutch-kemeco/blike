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
 * @file drvgtk_malloc.h
 * @brief メモリーアロケーター
 * @par アライメント調整済みのメモリー確保・開放関連の関数群です。
 */

#pragma once

#include <glib.h>

/** @fn gpointer drvgtk_malloc_aligned16(const gsize bytes)
 * @brief メモリーアドレスのアライメントが16Byte単位となるように、先頭アドレスを16で割り切れるアドレスとしてメモリーを確保します。
 * @param (const gsize bytes): 確保するメモリーサイズ（バイト単位）
 * @return (gpointer): 確保したメモリーの先頭アドレス
 */
gpointer drvgtk_malloc_aligned16(const gsize bytes);

/** @fn gpointer drvgtk_malloc_0_aligned16(const gsize bytes)
 * @brief メモリーアドレスのアライメントが16Byte単位となるように、先頭アドレスを16で割り切れるアドレスとしてメモリーを確保し、内容を0でクリアーします。
 * @param (const gsize bytes): 確保するメモリーサイズ（バイト単位）
 * @return (gpointer): 確保したメモリーの先頭アドレス
 */
gpointer drvgtk_malloc_0_aligned16(const gsize bytes);

/** @fn void drvgtk_free_aligned16(const gpointer a)
 * @brief メモリーを開放します。通常のfree()関数のラッパー関数です。
 * @param (const gpointer a): 開放するメモリーの先頭アドレス
 */
void drvgtk_free_aligned16(const gpointer a);

/** @fn gpointer drvgtk_malloc_rwe(const gsize bytes)
 * @brief メモリーアドレスのアライメントが16Byte単位となるように、先頭アドレスを16で割り切れるアドレスとしてメモリーを確保し、メモリー属性に「実行許可」のパーミッションを付加して返します。
 * @param (const gsize bytes): 確保するメモリーサイズ（バイト単位）
 * @return (gpointer): 確保したメモリーの先頭アドレス
 */
gpointer drvgtk_malloc_rwe(const gsize bytes);
