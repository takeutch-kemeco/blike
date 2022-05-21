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

#include <stdio.h>
#include <blike0.h>
#include "IO_BLIKE.h"

void sync_screen(void);

void vsync(int w)
{
	sync_screen();
	bl_wait(33 * w);
}

static int old_set_put_c_s_color  = 0;
static int old_set_put_c_bg_color = 0;

void set_put_c_color(int s_color, int bg_color)
{
	switch(s_color){
	case _COLOR_BLACK:	old_set_put_c_s_color = 0x000000; break;
	case _COLOR_RED:	old_set_put_c_s_color = 0xFF0000; break;
	case _COLOR_GREEN:	old_set_put_c_s_color = 0x00FF00; break;
	case _COLOR_YELLO:	old_set_put_c_s_color = 0xFFFF00; break;
	case _COLOR_BLUE:	old_set_put_c_s_color = 0x0000FF; break;
	case _COLOR_PARPLE:	old_set_put_c_s_color = 0xFF00FF; break;
	case _COLOR_LBLUE:	old_set_put_c_s_color = 0x7F7FFF; break;
	case _COLOR_WHITE:	old_set_put_c_s_color = 0xFFFFFF; break;
	case _COLOR_ORENGE:	old_set_put_c_s_color = 0xFF7F7F; break;
	default: old_set_put_c_s_color = 0x000000; break;
	}

	switch(bg_color){
	case _COLOR_BLACK:	old_set_put_c_bg_color = 0x000000; break;
	case _COLOR_RED:	old_set_put_c_bg_color = 0xFF0000; break;
	case _COLOR_GREEN:	old_set_put_c_bg_color = 0x00FF00; break;
	case _COLOR_YELLO:	old_set_put_c_bg_color = 0xFFFF00; break;
	case _COLOR_BLUE:	old_set_put_c_bg_color = 0x0000FF; break;
	case _COLOR_PARPLE:	old_set_put_c_bg_color = 0xFF00FF; break;
	case _COLOR_LBLUE:	old_set_put_c_bg_color = 0x7F7FFF; break;
	case _COLOR_WHITE:	old_set_put_c_bg_color = 0xFFFFFF; break;
	case _COLOR_ORENGE:	old_set_put_c_bg_color = 0xFF7F7F; break;
	default: old_set_put_c_bg_color = 0x000000; break;
	}
}

static int old_set_cur_x = 0;
static int old_set_cur_y = 0;

void set_cur(int x, int y)
{
	old_set_cur_x = x;
	old_set_cur_y = y;
}

void clr_scr(void)
{
	bl_setCol(0);
	bl_fillRect(640, 400, 0, 0);
}

void put_c(unsigned char c)
{
	const int block_w = 8;
	const int block_h = 16;

	if((old_set_cur_x>=0&&old_set_cur_x<80)&&(old_set_cur_y>=0&&old_set_cur_y<25)){
		bl_setCol(old_set_put_c_s_color);
//		bl_setCol(old_set_put_c_bg_color);

		int x = old_set_cur_x * block_w;
		int y = old_set_cur_y * block_h;
		int w = block_w;
		int h = block_h;

		bl_fillRect(w, h, x, y);

//		printf("%d, %d\n", old_set_put_c_s_color, old_set_put_c_bg_color);
	}
}

#define SCREEN_BUF_W	80
#define SCREEN_BUF_H	25

static unsigned char SCREEN_BUF_CH[SCREEN_BUF_W*SCREEN_BUF_H];
static int SCREEN_BUF_SC[SCREEN_BUF_W*SCREEN_BUF_H];
static int SCREEN_BUF_BC[SCREEN_BUF_W*SCREEN_BUF_H];

static unsigned char PUT_SCREEN_BUF_CH[SCREEN_BUF_W*SCREEN_BUF_H];
static int PUT_SCREEN_BUF_SC[SCREEN_BUF_W*SCREEN_BUF_H];
static int PUT_SCREEN_BUF_BC[SCREEN_BUF_W*SCREEN_BUF_H];
static int PUT_SCREEN_BUF_DF[SCREEN_BUF_W*SCREEN_BUF_H];

void sync_screen(void)
{
	int i,j;

	unsigned char*	psb_ch = PUT_SCREEN_BUF_CH;
	int* psb_sc = PUT_SCREEN_BUF_SC;
	int* psb_bc = PUT_SCREEN_BUF_BC;
	int* psb_df = PUT_SCREEN_BUF_DF;

	unsigned char* 	sb_ch = SCREEN_BUF_CH;
	int* sb_sc = SCREEN_BUF_SC;
	int* sb_bc = SCREEN_BUF_BC;

	for(j=0;j<SCREEN_BUF_H;j++){
		for(i=0;i<SCREEN_BUF_W;i++){
			*psb_df++ = 0;
		}
	}
	psb_df = PUT_SCREEN_BUF_DF;

	for(j=0;j<SCREEN_BUF_H;j++){
		for(i=0;i<SCREEN_BUF_W;i++){
			if(*psb_ch != *sb_ch){ *psb_ch = *sb_ch; *psb_df=1; }
			if(*psb_sc != *sb_sc){ *psb_sc = *sb_sc; *psb_df=1; }
			if(*psb_bc != *sb_bc){ *psb_bc = *sb_bc; *psb_df=1; }

			psb_ch++;
			psb_sc++;
			psb_bc++;
			sb_ch++;
			sb_sc++;
			sb_bc++;
			psb_df++;
		}
	}

	psb_ch = PUT_SCREEN_BUF_CH;
	psb_sc = PUT_SCREEN_BUF_SC;
	psb_bc = PUT_SCREEN_BUF_BC;
	psb_df = PUT_SCREEN_BUF_DF;

	for(j=0;j<SCREEN_BUF_H;j++){
		for(i=0;i<SCREEN_BUF_W;i++){
			if( *psb_df != 0 ){
				set_cur(i, j);
				set_put_c_color(*psb_sc, *psb_bc);
				put_c(*psb_ch);
			}

			psb_ch++;
			psb_sc++;
			psb_bc++;
			psb_df++;
		}
	}
}

int get_c(void)
{
	return bl_inkey1();
}

void init_sys(void)
{
        bl_openWin(640, 400);
	bl_srand(bl_clock());
}

int get_rand(void)
{
	return bl_rand();
}
