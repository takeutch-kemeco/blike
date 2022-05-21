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
#include "MINO.h"
#include "PANEL.h"
#include "TETO.h"

void initMINO( MINO* m, int t, POS* cmp )
{
	register BLOCK* bp = m->b;

	m->p.x = cmp->x;
	m->p.y = cmp->y;
	
	m->cp.x=0;
	m->cp.y=0;
		
	initB(bp++);	initB(bp++);	initB(bp++);	initB(bp++);

	bp = m->b;	
	switch(t) {
	case 0:	colB(bp++,1);	colB(bp++,1);	colB(bp++,1);	colB(bp++,1);	break;
	case 1:	colB(bp++,2);	colB(bp++,2);	colB(bp++,2);	colB(bp++,2);	break;
	case 2:	colB(bp++,3);	colB(bp++,3);	colB(bp++,3);	colB(bp++,3);	break;
	case 3:	colB(bp++,4);	colB(bp++,4);	colB(bp++,4);	colB(bp++,4);	break;
	case 4:	colB(bp++,5);	colB(bp++,5);	colB(bp++,5);	colB(bp++,5);	break;
	case 5:	colB(bp++,6);	colB(bp++,6);	colB(bp++,6);	colB(bp++,6);	break;
	case 6:	colB(bp++,7);	colB(bp++,7);	colB(bp++,7);	colB(bp++,7);	break;
	}	
	
	bp = m->b;	
	switch(t) {
	case 0:	posB(bp++,0,-1);	posB(bp++,0,0);		posB(bp++,0,1);		posB(bp++,0,2);		break;		// ----
	case 1:	posB(bp++,0,0);		posB(bp++,0,1);		posB(bp++,1,0);		posB(bp++,1,1);		break;		// []
	case 2:	posB(bp++,-1,0);	posB(bp++,0,0);		posB(bp++,1,0);		posB(bp++,0,-1);	break;		// T
	case 3:	posB(bp++,-1,0);	posB(bp++,0,0);		posB(bp++,0,1);		posB(bp++,1,1);		break;		// N
	case 4:	posB(bp++,-1,1);	posB(bp++,0,1);		posB(bp++,0,0);		posB(bp++,1,0);		break;		// !N
	case 5:	posB(bp++,0,-1);	posB(bp++,0,0);		posB(bp++,0,1);		posB(bp++,1,1);		break;		// L
	case 6:	posB(bp++,0,-1);	posB(bp++,0,0);		posB(bp++,0,1);		posB(bp++,-1,1);	break;		// !L
	}
	
	m->t=t;
}

void initGHOSTMINO( MINO* m, int t, POS* cmp )
{
	initMINO(m,t,cmp);

	register BLOCK* bp = m->b;

	colGB(bp);	bp->b[0]='[';	bp->b[1]=']';	bp++;
	colGB(bp);	bp->b[0]='[';	bp->b[1]=']';	bp++;
	colGB(bp);	bp->b[0]='[';	bp->b[1]=']';	bp++;
	colGB(bp);	bp->b[0]='[';	bp->b[1]=']';	bp++;
}

void cpyMINO( MINO* dst, MINO* src )
{
	dst->t = src->t;
	
	dst->p.x = src->p.x;
	dst->p.y = src->p.y;

	dst->cp.x = src->cp.x;
	dst->cp.y = src->cp.y;

	BLOCK* dbp = dst->b;
	BLOCK* sbp = src->b;
	cpyB(dbp++,sbp++);	cpyB(dbp++,sbp++);	cpyB(dbp++,sbp++);	cpyB(dbp++,sbp++);
}

int apMINO( MINO* m, PANEL* pp )
{
	int i;
	int x=m->p.x+m->cp.x;
	int y=m->p.y+m->cp.y;
	int ret=0;
	BLOCK* bp = m->b;
	ret+=apB( bp++,x,y, pp );	ret+=apB( bp++,x,y, pp );	ret+=apB( bp++,x,y, pp );	ret+=apB( bp++,x,y, pp );
	
	return(ret);
}

void __rotMINO( MINO* m, int r )
{
	BLOCK* bp = m->b;
	
	if(r >= +1){
		posB(bp, +bp->p.y,-bp->p.x ); bp++;
		posB(bp, +bp->p.y,-bp->p.x ); bp++;
		posB(bp, +bp->p.y,-bp->p.x ); bp++;
		posB(bp, +bp->p.y,-bp->p.x );
	}
	else if( r <= -1){
		posB(bp, -bp->p.y,+bp->p.x ); bp++;
		posB(bp, -bp->p.y,+bp->p.x ); bp++;
		posB(bp, -bp->p.y,+bp->p.x ); bp++;
		posB(bp, -bp->p.y,+bp->p.x );		
	}
}

int __movMINO( MINO* m, int x, int y, PANEL* pp )
{
	m->p.x += x;
	m->p.y += y;
	
	if(apMINO(m,pp)!=0){
		m->p.x -= x;	m->p.y -= y;
		return(1);
	}
	m->p.x += m->cp.x;	m->p.y += m->cp.y;
	m->cp.x=0;			m->cp.y=0;
	return(0);
}

void rot_rMINO( MINO* m, PANEL* pp )
{
	if(m->t==1){return;}

	__rotMINO( m, -1 );

	m->cp.x=0;	m->cp.y=0;
	if(apMINO(m,pp)!=0){
		m->cp.x=0;
		m->cp.y=-1; if(apMINO(m,pp)==0){ return; }

		m->cp.y=0;
		m->cp.x=+1; if(apMINO(m,pp)==0){ return; }
		m->cp.x=-1; if(apMINO(m,pp)==0){ return; }

		if(m->t==0){
			m->cp.x=0;
			m->cp.y=-2; if(apMINO(m,pp)==0){ return; }

			m->cp.y=0;
			m->cp.x=+2; if(apMINO(m,pp)==0){ return; }
			m->cp.x=-2; if(apMINO(m,pp)==0){ return; }
		}
			
		m->cp.x=0;	m->cp.y=0;
			
		__rotMINO( m, +1 );
	}
}

void rot_lMINO( MINO* m, PANEL* pp )
{
	if(m->t==1){return;}
  
	__rotMINO( m, +1 );	

	m->cp.x=0;	m->cp.y=0;
	if(apMINO(m,pp)!=0){
		m->cp.x=0;
		m->cp.y=-1; if(apMINO(m,pp)==0){ return; }

		m->cp.y=0;
		m->cp.x=+1; if(apMINO(m,pp)==0){ return; }
		m->cp.x=-1; if(apMINO(m,pp)==0){ return; }

		if(m->t==0){
			m->cp.x=0;
			m->cp.y=-2; if(apMINO(m,pp)==0){ return; }

			m->cp.y=0;
			m->cp.x=+2; if(apMINO(m,pp)==0){ return; }
			m->cp.x=-2; if(apMINO(m,pp)==0){ return; }
		}
		
		m->cp.x=0;	m->cp.y=0;
		__rotMINO( m, -1 );
	}
}

int mov_dMINO( MINO* m, PANEL* pp )
{
	return(__movMINO(m,0,1,pp));
}

int mov_rMINO( MINO* m, PANEL* pp )
{
	return(__movMINO(m,+1,0,pp));
}

int mov_lMINO( MINO* m, PANEL* pp )
{
	return(__movMINO(m,-1,0,pp));
}

void prtMINO( MINO* m, PANEL* pp )
{
	BLOCK* mbp = m->b;
	int x = pp->p.x + m->p.x + m->cp.x;
	int y = pp->p.y + m->p.y + m->cp.y;

	prtB(mbp,x,y); mbp++;
	prtB(mbp,x,y); mbp++;
	prtB(mbp,x,y); mbp++;
	prtB(mbp,x,y); mbp++;
}

void clrMINO( MINO* m, PANEL* pp )
{
	BLOCK* mbp = m->b;
	int x = pp->p.x + m->p.x + m->cp.x;
	int y = pp->p.y + m->p.y + m->cp.y;

	clrB(mbp,x,y); mbp++;
	clrB(mbp,x,y); mbp++;
	clrB(mbp,x,y); mbp++;
	clrB(mbp,x,y); mbp++;
}

void prtGHOSTMINO( MINO* gm, MINO* cur_mino, PANEL* pp )
{
	clrMINO(gm,pp);
  
	gm->p.x  = cur_mino->p.x + cur_mino->cp.x;
 	gm->p.y	 = cur_mino->p.y + cur_mino->cp.y;
	gm->cp.x = 0;
	gm->cp.y = 0;
	
	int i;
	
	for(i=0; i<PANEL_H; i++){
		if(mov_dMINO(gm,pp)!=0){break;}
	}

	prtMINO(gm,pp);
}

void M2P( MINO* m, PANEL* pp )
{
	int i;
	BLOCK* mbp = m->b;
	BLOCK* pbp;

	for(i=0;i<4;i++){
		mbp = &(m->b[i]);

		mbp->p.x += m->p.x + m->cp.x;
		mbp->p.y += m->p.y + m->cp.y;

		pbp = &(pp->b[ mbp->p.y*PANEL_W +mbp->p.x ]);
		cpyB(pbp, mbp);
	}
}
