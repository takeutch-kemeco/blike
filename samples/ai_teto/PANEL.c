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
#include "POS.h"
#include "BLOCK.h"
#include "MINO.h"
#include "PANEL.h"

void initP(PANEL* pp, int x, int y)
{
	int i,j;
	BLOCK* bp;
	
	pp->p.x=x;
	pp->p.y=y;
	
	for(j=0;j<PANEL_H;j++) {
		i=0;
		bp=&(pp->b[j*PANEL_W]);
	  
		switch(j){
		case PANEL_H-1:
			for(i=-0;i<PANEL_W;i++) {
				initB(bp); posB(bp,i,j); colB(bp,8); bp++;
			}
			break;

		default:
			initB(bp); posB(bp,i,j); colB(bp,8); bp++;
			for(i=1;i<PANEL_W-1;i++) {
				initB(bp); posB(bp,i,j); colB(bp,0); bp++;
			}
			initB(bp); posB(bp,i,j); colB(bp,8); bp++;
			break;
		}
	}	
}

void initNP( NPANEL* npp, int x, int y)
{
	int i,j;
	BLOCK* bp;
	
	npp->p.x=x;
	npp->p.y=y;

	for(j=0;j<NPANEL_H;j++) {
		i=0;
		bp=&(npp->b[j*NPANEL_W]);
	  
		switch(j){
		case 0: case NPANEL_H-1:
			for(i=-0;i<NPANEL_W;i++) {
				initB(bp); posB(bp,i,j); colB(bp,8); bp++;
			}
			break;

		default:
			initB(bp); posB(bp,i,j); colB(bp,8); bp++;
			for(i=1;i<NPANEL_W-1;i++) {
				initB(bp); posB(bp,i,j); colB(bp,0); bp++;
			}
			initB(bp); posB(bp,i,j); colB(bp,8); bp++;
			break;
		}
	}	
}

int chkP( PANEL* pp, int* comp_line )
{
	int j,i;
	int c;
	int ret =0;
	BLOCK* bp;
	
	for(j=0;j<PANEL_H-1;j++){
		bp=&(pp->b[j*PANEL_W+1]);
		c=0;
		for(i=1;i<PANEL_W-1;i++){
			if( bp->bg_col != _COLOR_BLACK ){ c++; }
			bp++;
		}
		if(c==PANEL_W-1-1) {
			comp_line[j]=1;
			ret++;
		}
		else {
			comp_line[j]=0;
		}
	}
	
	return(ret);
}

int flsP( PANEL* pp, int* cur_flsP, int* comp_line )
{
	int i,j;
	BLOCK* bp;
	int ret = 0;
	
	int col;
	switch( *cur_flsP ) {
	case 0: col=_COLOR_LBLUE;	*cur_flsP=1;	break;
	case 1: col=_COLOR_YELLO; 	*cur_flsP=0;	break;
	}
	  
	for(j=0;j<PANEL_H-1;j++){
		if( comp_line[j]!=0 ){
			bp = &(pp->b[j*PANEL_W+1]);
			for(i=1;i<PANEL_W-1;i++){
				bp->str_col = col;
				bp->bg_col  = col;
				bp++;
			}
		}
	}

	return(ret);
}

void dllP( PANEL* pp, int* comp_line )
{
	int i,j;
	BLOCK* bp;
	
	for(j=0;j<PANEL_H-1;j++){
		if( comp_line[j]!=0 ){
			bp = &(pp->b[j*PANEL_W+1]);
			for(i=1;i<PANEL_W-1;i++){
				bp->str_col = _COLOR_BLACK;
				bp->bg_col  = _COLOR_BLACK;
				bp++;
			}
		}
	}
}

int dwnP( PANEL* pp )
{
	int i,j;
	BLOCK* bp;
	BLOCK* ubp;
	int c;
	int ret=0;
	
	for(j=PANEL_H-1-1; j>0; j--){
		bp = &(pp->b[j*PANEL_W+1]);
		c=0;
		for(i=1;i<PANEL_W-1;i++){
			if( bp->bg_col == _COLOR_BLACK ) { c++; };
			bp++;
		}
		if(c==PANEL_W-1-1){
			bp  = &(pp->b[(j+0)*PANEL_W+1]);
			ubp = &(pp->b[(j-1)*PANEL_W+1]);
			for(i=1;i<PANEL_W-1;i++){
				bp->str_col = ubp->str_col;
				bp->bg_col  = ubp->bg_col;
				ubp->str_col = _COLOR_BLACK;
				ubp->bg_col  = _COLOR_BLACK;
				bp++;
				ubp++;
			}
			
			ret++;
		}
	}
	
	return(ret);
}

void ojlP( PANEL* pp, int ojls )
{
	int i,j;
	BLOCK* bp;
	BLOCK* ubp;
	
	for(j=1;j<PANEL_H;j++){
		ubp = &(pp->b[(j-1)*PANEL_W+1]);
		bp  = &(pp->b[(j+0)*PANEL_W+1]);
		for(i=1;i<PANEL_W-1;i++){
			ubp->str_col = bp->str_col;
			ubp->bg_col  = bp->bg_col;
			bp++;
			ubp++;
		}
	}
	
	bp  = &(pp->b[(PANEL_H-1-1)*PANEL_W+1]);
	for(i=1;i<PANEL_W-1;i++){
		bp->str_col = _COLOR_WHITE;
		bp->bg_col  = _COLOR_WHITE;
		bp++;
	}	

	bp = &(pp->b[(PANEL_H-1-1)*PANEL_W+1]);
	bp += ojls;									// 一定の位置に穴
//	bp += rand()%(PANEL_W-1-1);					//　常にランダムに穴
	bp->str_col	= _COLOR_BLACK;
	bp->bg_col  = _COLOR_BLACK;
}

void prtP( PANEL* pp )
{
	int i,j;
	BLOCK* bp;
	
	for(j=0;j<PANEL_H;j++){
		i=0;
		bp=&(pp->b[j*PANEL_W]);
		
		for(i=0;i<PANEL_W;i++){
			  prtB(bp++, pp->p.x, pp->p.y);
		}
	}
}

void prtNP( NPANEL* npp, PANEL* pp )
{
	int i,j;
	BLOCK* bp;

	for(j=0;j<NPANEL_H;j++){
		i=0;
		bp=&(npp->b[j*NPANEL_W]);
		
		for(i=0;i<NPANEL_W;i++){
			  prtB( bp++, pp->p.x + npp->p.x, pp->p.y + npp->p.y );
		}
	}
}

int apB( BLOCK* bp, int x, int y, PANEL* pp )
{
	int cx=bp->p.x+x;
	int cy=bp->p.y+y;

	switch( pp->b[cy*PANEL_W+cx].bg_col ){
	case _COLOR_BLACK:	return(0);	break;
	default:			return(1);	break;
	}
}
