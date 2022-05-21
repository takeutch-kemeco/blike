/*
 * 3-clause BSD license
 * Copyright (c) 2011, Kemeco
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *      * Redistributions of source code must retain the above copyright
 *        notice, this list of conditions and the following disclaimer.
 *      * Redistributions in binary form must reproduce the above copyright
 *        notice, this list of conditions and the following disclaimer in the
 *        documentation and/or other materials provided with the distribution.
 *      * Neither the name of the Kemeco nor the
 *        names of its contributors may be used to endorse or promote products
 *        derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY Kemeco ''AS IS'' AND ANY
 * EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL Kemeco BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include "PAD.h"
#include "IO_BLIKE.h"

unsigned short PAD1;
unsigned short PAD2;

void syncPAD(void)
{
	PAD1 = 0;
	PAD2 = 0;

	int c;

	do {
		c = get_c();

		switch (c) {
		case 'f': case 'F': 	PAD1 |= PAD_R_UP;	break; // プレステの○ボタン相当。右回転
		case 'd': case 'D':	PAD1 |= PAD_R_LEFT;	break; // プレステの❌ボタン相当。左回転
		case KEY_RIGHT:		PAD1 |= PAD_L_RIGHT;	break; // プレステの十字キー右相当。右移動
		case KEY_LEFT:		PAD1 |= PAD_L_LEFT;	break; // プレステの十字キー左相当。左移動
		case KEY_DOWN:		PAD1 |= PAD_L_DOWN;	break; // プレステの十字キー下相当。下移動
		case ' ':		PAD1 |= PAD_L_UP;	break; // ドロップ
		case 'q': case 'Q':	PAD1 |= PAD_START;	break; // プレステのスタートキー相当。リセットしてリスタート
		case 'k': case 'K':	PAD1 |= PAD_L1;		break; // プレステのL1ボダン相当。CPUにタッチ
		case 'n': case 'N':	PAD1 |= PAD_R1;		break; // プレステのR1ボタン相当。人間にタッチ
		}
	} while (c != 0);
}
