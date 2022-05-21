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
#include "MINO.h"
#include "PANEL.h"
#include "GEN_MINO.h"
#include "AI.h"

#define NINGEN 		0
#define KIKAI  		1
#define MUJINKUN	2

typedef struct tagTETO {
	int		pmd;
	POS		nmp;
	POS		cmp;			// 現在のミノ位置
	PANEL 		P;
	NPANEL		NP;
	int 		comp_line[PANEL_H]; 	// = {0,0,0,0,0,	0,0,0,0,0,	0,0,0,0,0,	0,0,0,0,0,	0,0};
	int		cur_flsP;		// =0;

	GEN_MINO	gen_mino;
	
	AI	 	ai;
	int 		snc;			//=0;
	int 		f;			//=0;
	MINO 		cur_mino;
	MINO 		next_mino;
	MINO		cur_ghost_mino;
	MINO		next_ghost_mino;
	int		ojl;
	int		ojls;
	int		awt;
	int		cawt;
} TETO;

void initTETO(TETO* teto, int pmd, int x, int y, int nx, int ny );
int calcTETO(TETO* teto, unsigned short pad, int dp);
