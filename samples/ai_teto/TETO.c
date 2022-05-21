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
#include "PAD.h"
#include "TETO.h"

void initTETO(TETO* teto, int pmd, int x, int y, int nx, int ny )
{
	int i;
	
	teto->cur_flsP=0;
	
	for(i=0;i<PANEL_H;i++){teto->comp_line[i]=0;}

	initGEN_MINO( &teto->gen_mino, &teto->cmp );
	
	teto->snc=0;
	teto->f=0;
	
	teto->pmd=pmd;
	teto->nmp.x=nx; teto->nmp.y=ny;
	teto->cmp.x=6;  teto->cmp.y=3;
	
	initP(&teto->P,x,y);
	initNP(&teto->NP,nx-2,ny-2);
	
	newMINO( &teto->next_mino, &teto->next_ghost_mino, &teto->gen_mino );
	cpyMINO( &teto->cur_mino, 		&teto->next_mino );
	teto->cur_mino.p.x= teto->cmp.x;	teto->cur_mino.p.y= teto->cmp.y;
	cpyMINO( &teto->cur_ghost_mino,	&teto->next_ghost_mino );

	newMINO( &teto->next_mino, &teto->next_ghost_mino, &teto->gen_mino );
	teto->next_mino.p.x= teto->nmp.x;	teto->next_mino.p.y= teto->nmp.y;
	
	teto->ojl=0;
	teto->ojls=get_rand()%(PANEL_W-1-1);

	teto->awt  = 60;	// 1s
	teto->cawt = 0;
	
	PAD1=0;
	PAD2=0;
}

int calcTETO( TETO* teto, unsigned short pad, int dp )
{
	int ret = 0;
	
	if(teto->pmd==MUJINKUN){return(0);}
	
	teto->cur_ghost_mino.p.y  = teto->cur_mino.p.y;
	teto->cur_ghost_mino.p.x  = teto->cur_mino.p.x;
	teto->cur_ghost_mino.cp.y = teto->cur_mino.cp.y;
	teto->cur_ghost_mino.cp.x = teto->cur_mino.cp.x;
	prtGHOSTMINO( &teto->cur_ghost_mino, &teto->cur_mino, &teto->P );
	prtMINO( &teto->cur_mino, &teto->P );

	if(teto->pmd==KIKAI){
		if(teto->f==0&&teto->snc==0){
			sncAI( &teto->ai, &teto->cur_mino, &teto->P );	teto->snc=1;
		}

		if(teto->snc==1){
			if(teto->ai.CTR>=1){ teto->ai.CTR--; pad|=PAD_R_LEFT;}
			else if(teto->ai.CTH>=+1){ teto->ai.CTH--; pad|=PAD_L_RIGHT;}
			else if(teto->ai.CTH<=-1){ teto->ai.CTH++; pad|=PAD_L_LEFT;}
			else { pad|=PAD_L_DOWN;} // 弱い
//			else { pad|=PAD_L_UP;} // 強い
		}
	}
	
	if(teto->f==0){
		if( ((pad&PAD_L_DOWN)!=0) || dp==1 || ((pad&PAD_L_UP)!=0) ){ 
			clrMINO( &teto->cur_mino, &teto->P );

			if( (pad&PAD_L_UP)!=0 ) {
				while( mov_dMINO( &teto->cur_mino, &teto->P )==0 ){;}
			}
	
			if( mov_dMINO( &teto->cur_mino, &teto->P )!=0 ){
				teto->cawt++;
				if((pad&PAD_L_UP)!=0){ teto->cawt = teto->awt; }
			}
			
			prtMINO( &teto->cur_mino, &teto->P );
		}
		
		if(teto->cawt > 0){
			clrMINO( &teto->cur_mino, &teto->P );

			if( mov_dMINO( &teto->cur_mino, &teto->P )!=0 ){
				if(teto->cawt >= teto->awt){
					teto->cawt=0;
				  
					if(teto->ojl>0){teto->f=30;}
				  
					teto->snc=0;
					M2P( &teto->cur_mino, &teto->P );
					prtP( &teto->P );
									
					clrMINO( &teto->next_mino, &teto->P );
					cpyMINO( &teto->cur_mino, &teto->next_mino );	teto->cur_mino.p.x = teto->cmp.x;	teto->cur_mino.p.y = teto->cmp.y;
					cpyMINO( &teto->cur_ghost_mino, &teto->next_ghost_mino );
					
					newMINO( &teto->next_mino, &teto->next_ghost_mino, &teto->gen_mino );
					teto->next_mino.p.x= teto->nmp.x;	teto->next_mino.p.y= teto->nmp.y;
					
					prtMINO( &teto->next_mino, &teto->P );
				}
				else{
					teto->cawt++;
				}
			}
			else{
				teto->cawt=0;
			}
			
			prtMINO( &teto->cur_mino, &teto->P );
		}
			
		if(pad&PAD_L_LEFT ){
			clrMINO( &teto->cur_mino, &teto->P );	mov_lMINO( &teto->cur_mino, &teto->P );
			prtGHOSTMINO( &teto->cur_ghost_mino, &teto->cur_mino, &teto->P );	prtMINO( &teto->cur_mino, &teto->P );
		}
		if(pad&PAD_L_RIGHT){
			clrMINO( &teto->cur_mino, &teto->P );	mov_rMINO( &teto->cur_mino, &teto->P );
			prtGHOSTMINO( &teto->cur_ghost_mino, &teto->cur_mino, &teto->P );	prtMINO( &teto->cur_mino, &teto->P );
		}
		
		if(pad&PAD_R_UP   ){
			clrMINO( &teto->cur_mino, &teto->P );				clrMINO( &teto->cur_ghost_mino, &teto->P );
			teto->cur_ghost_mino.p.y  = teto->cur_mino.p.y;
			teto->cur_ghost_mino.p.x  = teto->cur_mino.p.x;
			teto->cur_ghost_mino.cp.y = teto->cur_mino.cp.y;
			teto->cur_ghost_mino.cp.x = teto->cur_mino.cp.x;
			rot_rMINO( &teto->cur_mino, &teto->P );			rot_rMINO( &teto->cur_ghost_mino, &teto->P );
			prtGHOSTMINO( &teto->cur_ghost_mino, &teto->cur_mino, &teto->P );	prtMINO( &teto->cur_mino, &teto->P );
		}
		if(pad&PAD_R_LEFT ){
			clrMINO( &teto->cur_mino, &teto->P );				clrMINO( &teto->cur_ghost_mino, &teto->P );
			teto->cur_ghost_mino.p.y  = teto->cur_mino.p.y;
			teto->cur_ghost_mino.p.x  = teto->cur_mino.p.x;
			teto->cur_ghost_mino.cp.y = teto->cur_mino.cp.y;
			teto->cur_ghost_mino.cp.x = teto->cur_mino.cp.x;
			rot_lMINO( &teto->cur_mino, &teto->P );			rot_lMINO( &teto->cur_ghost_mino, &teto->P );
			prtGHOSTMINO( &teto->cur_ghost_mino, &teto->cur_mino, &teto->P );	prtMINO( &teto->cur_mino, &teto->P );
		}
	}
	
	if(teto->f==0){
		ret=chkP( &teto->P, teto->comp_line ); if(ret!=0){teto->f=1;}
	}
	else if(teto->f>=1&&teto->f<=6){
		flsP( &teto->P, &teto->cur_flsP, teto->comp_line );	teto->f++;
		prtP( &teto->P ); prtMINO( &teto->cur_mino, &teto->P );
	}
	else if(teto->f==7) {
		dllP( &teto->P, teto->comp_line ); prtP( &teto->P ); prtMINO( &teto->cur_mino, &teto->P ); teto->f++;
	}
	else if(teto->f>=8&&teto->f<=11) {
		dwnP( &teto->P ); prtP( &teto->P ); prtMINO( &teto->cur_mino, &teto->P ); teto->f++;
	}
	else if(teto->f==12){
		prtP( &teto->P );

		teto->cur_ghost_mino.p.y  = teto->cur_mino.p.y;
		teto->cur_ghost_mino.p.x  = teto->cur_mino.p.x;
		teto->cur_ghost_mino.cp.y = teto->cur_mino.cp.y;
		teto->cur_ghost_mino.cp.x = teto->cur_mino.cp.x;
		prtGHOSTMINO( &teto->cur_ghost_mino, &teto->cur_mino, &teto->P );	prtMINO( &teto->cur_mino, &teto->P );

		teto->f=0;
	}
	
	if(teto->f==30){
		ojlP( &teto->P, teto->ojls );
		prtP( &teto->P );
		prtMINO( &teto->cur_mino, &teto->P );
		teto->ojl--;
		if(teto->ojl<=0){teto->f=0;}

		ret=chkP( &teto->P, teto->comp_line );
		if(ret!=0){teto->f=1;}
	}
	
	return(ret);
}
