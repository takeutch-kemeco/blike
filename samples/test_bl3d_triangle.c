#include "blike.h"

#include "bl3d.h"
#include "bl_load_xpm.h"

static struct BL3D_OT ot;

struct BL3D_TRIANGLE_G_T model_data[] = {
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.vertex[0]={-100,-100,0},
		.vertex[1]={200,200,0},
		.vertex[2]={-100,200,0},
		.texture[0]={0,0},
		.texture[1]={800,800},
		.texture[2]={0,800},
		.color[0]={0.0,0.0,0.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.vertex[0]={-100,-100,0},
		.vertex[1]={200,-100,0},
		.vertex[2]={200,200,0},
		.texture[0]={0,0},
		.texture[1]={800,0},
		.texture[2]={800,800},
		.color[0]={0.0,0.0,0.0},
		.color[1]={0.0,0.0,0.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.vertex[0]={0,0,50},
		.vertex[1]={300,300,100},
		.vertex[2]={0,300,120},
		.texture[0]={0,0},
		.texture[1]={800,800},
		.texture[2]={0,800},
		.color[0]={0.0,0.0,0.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
};



#include "test_h.xpm"

blMain()
{
	openWin(480, 270);
	
	
	
	const int vram_width  = 1400;
	const int vram_height = 1400;
	const int vram = 10;
	openVWin(vram, vram_width, vram_height);

	slctWin(vram);			// 描画先の画面を、裏画面１０にする。
	bl_load_xpm(test_h_xpm, 0, 0);	// 裏画面１０のx,y位置へ、xpm画像をロードする。

	
	
	struct BL3D_DOBJ dobj;
	bl3d_link_object(&dobj, model_data, 2);
	
	
	
	struct BL3D_MATRIX m0,m1,m2;
	m0=bl3d_e_matrix;
	m1=bl3d_e_matrix;
	m2=bl3d_e_matrix;
	bl3d_mul_matrix(&m2,&m0,&m1);
	bl3d_print_matrix(&m2);
	
	struct BL3D_VECTOR rotate ={0,0,0};
	bl3d_rot_matrix(&m2, &rotate);
	bl3d_print_matrix(&m2);
	
	struct BL3D_COORDINATE coord = {
		.compleate_flg = 0,
		.rotate = {0,0,0},
		.transfer = {0,0,0},
		.super = NULL
	};
	bl3d_get_lws(&m2, &m1, &coord);
	bl3d_print_matrix(&m1);
	bl3d_print_matrix(&m2);

	
	
	setCol(0xffffff);
	slctWin(4);
	fillRect(480,270,0,0);

	

	bl3d_ws_matrix = bl3d_e_matrix;
	bl3d_ls_matrix = bl3d_e_matrix;

	

	dobj.local_coord.transfer.x = 480/2;
	dobj.local_coord.transfer.y = 270/2;
	dobj.local_coord.transfer.z = 1000;
	dobj.local_coord.compleate_flg = 0;

	
	
	while(1) {
		copyRct0(
			480, 270,
			4, 0, 0,
			0, 0, 0
		);
		
		
		
		dobj.local_coord.rotate.x += 0.01;
		dobj.local_coord.rotate.y += 0.02;
		dobj.local_coord.rotate.z += 0.03;
		dobj.local_coord.compleate_flg = 0;
		
		
		
		bl3d_clear_ot(&ot);
		bl3d_sort_object(&dobj, &ot);
		bl3d_draw_ot(&ot);
	
		wait(1000/24);
	}
}
