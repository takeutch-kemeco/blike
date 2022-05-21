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
#include "BLOCK.h"
#include "PANEL.h"
#include "AI.h"

int stAI( AI* ai, PANEL* pp )
{
	int i,j;
	int sc=0;
	int h;
	int nl=_COLOR_BLACK;
	BLOCK* bp;
	BLOCK* lbp;
	BLOCK* rbp;
	BLOCK* ubp;
	BLOCK* dbp;
	for(j=1;j<PANEL_H-1;j++){
		bp=&(pp->b[j*PANEL_W+1]);
		lbp=bp-1;
		rbp=bp+1;
		ubp=bp-PANEL_W;
		dbp=bp+PANEL_W;
		for(i=1;i<PANEL_W-1;i++){
			if(j>PANEL_H-6){
				if(bp->bg_col!=nl){
					h=(PANEL_H-j);
					sc+=h;
					if(lbp->bg_col!=nl){if(i>=2&&i<=PANEL_W-1){sc-=2;}}
					if(rbp->bg_col!=nl){if(i>=1&&i<=PANEL_W-2){sc-=2;}}
					if(lbp->bg_col!=nl){if(i==1)        {sc-=1;}}
					if(rbp->bg_col!=nl){if(i==PANEL_W-1){sc-=1;}}
					if(dbp->bg_col==nl){sc+=10;}
				}
				if(bp->bg_col==nl){
					if(lbp->bg_col!=nl&&ubp->bg_col!=nl){sc+=10;}
					if(rbp->bg_col!=nl&&ubp->bg_col!=nl){sc+=10;}
					if(lbp->bg_col!=nl&&rbp->bg_col!=nl&&ubp->bg_col==nl){sc-=1;};
					if(lbp->bg_col!=nl&&rbp->bg_col!=nl&&ubp->bg_col==nl&&dbp->bg_col==nl){sc-=1;};
				}
			}
			else {
				if(bp->bg_col!=nl){
					h=(PANEL_H-j);
					sc+=h*2;
					if(lbp->bg_col!=nl){if(i>=2&&i<=PANEL_W-1){sc-=2;}}
					if(rbp->bg_col!=nl){if(i>=1&&i<=PANEL_W-2){sc-=2;}}
					if(lbp->bg_col!=nl){if(i==1)        {sc-=2;}}
					if(rbp->bg_col!=nl){if(i==PANEL_W-1){sc-=2;}}
					if(dbp->bg_col==nl){sc+=10;}
				}
				if(bp->bg_col==nl){
					if(lbp->bg_col!=nl&&ubp->bg_col!=nl){sc+=10;}
					if(rbp->bg_col!=nl&&ubp->bg_col!=nl){sc+=10;}
					if(rbp->bg_col!=nl&&lbp->bg_col!=nl){sc+=5;}
//					if(lbp->bg_col!=nl&&rbp->bg_col!=nl&&ubp->bg_col==nl){sc-=1;};
//					if(lbp->bg_col!=nl&&rbp->bg_col!=nl&&ubp->bg_col==nl&&dbp->bg_col==nl){sc-=1;};
				}
			}

			bp++;
			lbp=bp-1;
			rbp=bp+1;
			ubp=bp-PANEL_W;
			dbp=bp+PANEL_W;
		}
	}
	
	return(sc);
}

void sncAI( AI* ai, MINO* cm, PANEL* pp )
{
  	PANEL* CPP = &(ai->CPP);
	MINO* CPM = &(ai->CPM);
	int* CTV = &(ai->CTV);
	int* CTH = &(ai->CTH);
	int* CTR = &(ai->CTR);

	int i,j,k,l;
	int ls=0xFFFFFFF;
	int sp=cm->p.x;
	int	tv=0,th=0,tr=0;
	BLOCK* pbp=pp->b;
	BLOCK* cbp=CPP->b;
	for(i=0;i<PANEL_H*PANEL_W;i++){*cbp++ = *pbp++;}
	cpyMINO( CPM, cm );

	for(l=0;l<4;l++){
		for(k=1;k<PANEL_W-1;k++){
			pbp=pp->b;
			cbp=CPP->b;
			for(i=0;i<PANEL_H*PANEL_W;i++){*pbp++ = *cbp++;}
			cpyMINO( cm, CPM );
			
			for(i=0;i<l;i++){ tr=l; rot_lMINO(cm,pp); }
			
			int hm=k-sp;
			th=0;
			if(hm<0){
				hm=-hm;
				for(i=0;i<hm;i++){ th--; mov_lMINO(cm,pp); }
			}
			else {
				for(i=0;i<hm;i++){ th++; mov_rMINO(cm,pp); }
			}
			tv=0;
			for(j=0;j<PANEL_H-1;j++){
				tv++;
				if(mov_dMINO(cm,pp)!=0){break;}
			}

			M2P(cm,pp);
			int st=stAI(ai,pp);
			if(st<ls){ls=st;*CTH=th;*CTV=tv;*CTR=tr;}
		}
	}
  
	pbp=pp->b;
	cbp=CPP->b;
	for(i=0;i<PANEL_H*PANEL_W;i++){*pbp++ = *cbp++;}
	cpyMINO( cm, CPM );
}
