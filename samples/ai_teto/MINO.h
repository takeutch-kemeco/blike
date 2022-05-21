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

#pragma once

#include "POS.h"
#include "BLOCK.h"
#include "PANEL.h"

typedef struct tagMINO {
	int t;
	BLOCK b[4];
	POS p;
	POS cp;
} MINO;

void initMINO(MINO *m, int t, POS *cmp);
void initGHOSTMINO(MINO *m, int t, POS *cmp);
void cpyMINO(MINO *dst, MINO *src);
int apMINO(MINO *m, PANEL *pp);

void __rotMINO(MINO* m, int r);
int __movMINO(MINO* m, int x, int y, PANEL* pp);

void rot_rMINO(MINO *m, PANEL *pp);
void rot_lMINO(MINO *m, PANEL *pp);

int mov_dMINO(MINO *m, PANEL *pp);
int mov_rMINO(MINO *m, PANEL *pp);
int mov_lMINO(MINO *m, PANEL *pp);

void prtMINO(MINO *m, PANEL *pp);
void clrMINO(MINO *m, PANEL *pp);
void prtGHOSTMINO(MINO *gm, MINO *cur_mino, PANEL *pp);

//void initDM(TETO *teto);
//void initGM(TETO *teto);
//void newMINO(TETO *teto, MINO *m, MINO *gm);

void M2P(MINO *m, PANEL *pp);
