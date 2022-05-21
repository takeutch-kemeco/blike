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

#include "IO_BLIKE.h"
#include "TETO.h"
#include "PAD.h"

int blMain()
{
	int e = 0;
	int i;

	TETO t[2];
	int dll[2];
	
	int wt=10;
	int cwt=0;
	int dp=0;
	
	init_sys();
	initTETO(&t[0], NINGEN, 1, -4, 15, 7 );
	initTETO(&t[1], KIKAI, 27, -4, -4, 7 );

	clr_scr();
	
	prtP(&t[0].P);
	prtP(&t[1].P);

	prtNP(&t[0].NP, &t[0].P);
	prtNP(&t[1].NP, &t[1].P);

	prtMINO(&t[0].next_mino, &t[0].P);
	prtMINO(&t[1].next_mino, &t[1].P);
	
	do {
		dll[0] = calcTETO(&t[0], PAD1, dp);
		dll[1] = calcTETO(&t[1], PAD2, dp);

		if (dll[0] >= 1) {
			t[1].ojl += dll[0] - 1;
		}

		if (dll[1] >= 1) {
			t[0].ojl += dll[1] - 1;
		}
		
		vsync(1);
		syncPAD();

		dp=0;
		if (cwt++ >= wt) {
			dp = 1;
			cwt = 0;
		}

		// 人間にタッチ
		if (PAD1 & PAD_R1) {
			t[0].pmd = NINGEN;
		}

		// CPUにタッチ
		if (PAD1 & PAD_L1) {
			t[0].pmd = KIKAI;
		}

		// リセット、リスタート
		if (PAD1 & PAD_START) {
			initTETO(&t[0], NINGEN, 1, -4, 15, 7 );
			initTETO(&t[1], KIKAI, 27, -4, -4, 7 );

			clr_scr();

			prtP(&t[0].P);
			prtP(&t[1].P);

			prtNP(&t[0].NP, &t[0].P);
			prtNP(&t[1].NP, &t[1].P);
		}
	} while (1);

	return 0;
}
