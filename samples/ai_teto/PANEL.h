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

#define PANEL_W		12
#define PANEL_H		25
typedef struct tagPANEL {
	BLOCK 	b[PANEL_W*PANEL_H*2];
	POS		p;
} PANEL;

#define NPANEL_W	5
#define NPANEL_H	6
typedef struct tagNPANEL {
	BLOCK	b[NPANEL_W*NPANEL_H*2];
	POS		p;
} NPANEL;

void initP(PANEL* pp, int x, int y);
void initNP( NPANEL* npp, int x, int y);
int chkP( PANEL* pp, int* comp_line );
int flsP( PANEL* pp, int* cur_flsP, int* comp_line );
void dllP( PANEL* pp, int* comp_line );
int dwnP( PANEL* pp );
void ojlP( PANEL* pp, int ojls );
void prtP( PANEL* pp );
void prtNP( NPANEL* npp, PANEL* pp );
int apB( BLOCK* bp, int x, int y, PANEL* pp );
