#include "blike.h"

#include "bl3d.h"
#include "bl_load_xpm.h"

static struct BL3D_OT ot;

static struct BL3D_TRIANGLE_G_T model_data[] = {
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.vertex[0]={-325/50,-551/50,0},
		.vertex[1]={325/50,551/50,0},
		.vertex[2]={-325/50,551/50,0},
		.texture[0]={0,0},
		.texture[1]={325,551},
		.texture[2]={0,551},
		.color[0]={0.8,0.8,0.8},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.vertex[0]={-325/50,-551/50,0},
		.vertex[1]={325/50,-551/50,0},
		.vertex[2]={325/50,551/50,0},
		.texture[0]={0,0},
		.texture[1]={325,0},
		.texture[2]={325,551},
		.color[0]={1.0,1.0,1.0},
		.color[1]={0.8,0.8,0.8},
		.color[2]={1.0,1.0,1.0},
	},
};

static struct BL3D_DOBJ dobj[1000];
static struct BL3D_VECTOR tt[1000];
static struct BL3D_VECTOR tr[1000];



#include "test_h.xpm"

blMain()
{
	openWin(480, 270);
	
	
	
	const int vram_width  = 1000;
	const int vram_height = 1000;
	const int vram = 10;
	openVWin(vram, vram_width, vram_height);

	slctWin(vram);			// 描画先の画面を、裏画面１０にする。
	bl_load_xpm(test_h_xpm, 0, 0);	// 裏画面１０のx,y位置へ、xpm画像をロードする。

	
	
	int i;
	const int dobj_len = 100;
	for(i=0;i<dobj_len;i++) {
		bl3d_link_object(&dobj[i], model_data, 2);

		dobj[i].local_coord.transfer.x = 0 + (i*4);
		dobj[i].local_coord.transfer.y = 100;
		dobj[i].local_coord.transfer.z = 0;
		
		tt[i].x = 1 + bl_rand() % 10;
		tt[i].y = 1 + bl_rand() % 10;
		
		tr[i].x = (bl_rand() % 100) / 1000.0;
		tr[i].y = (bl_rand() % 100) / 1000.0;
		tr[i].z = (bl_rand() % 100) / 1000.0;

		dobj[i].local_coord.compleate_flg = 0;
	}
	

	
	setCol(0xffffff);
	slctWin(4);
	fillRect(480,270,0,0);

	

	bl3d_ws_matrix = bl3d_e_matrix;
	bl3d_ls_matrix = bl3d_e_matrix;
	

	
	while(1) {
		copyRct0(
			480, 270,
			4, 0, 0,
			0, 0, 0
		);
		
		
		
		bl3d_clear_ot(&ot);
		
		for(i=0; i<dobj_len; i++) {
			dobj[i].local_coord.rotate.x += tr[i].x;
			dobj[i].local_coord.rotate.y += tr[i].y;
			dobj[i].local_coord.rotate.z += tr[i].z;

			if(dobj[i].local_coord.transfer.x >= 480){
				dobj[i].local_coord.transfer.x = 479;
				tt[i].x *= -1 * ((bl_rand() % 200)/100.0);
			}
			else if(dobj[i].local_coord.transfer.x <= 0){
				dobj[i].local_coord.transfer.x = 1;
				tt[i].x *= -1 * ((bl_rand() % 200)/100.0);
			}

			if(dobj[i].local_coord.transfer.y >= 270){
				dobj[i].local_coord.transfer.y = 269;
				tt[i].y *= -1 * ((bl_rand() % 200)/100.0);
			}
			else if(dobj[i].local_coord.transfer.y <= 0){
				dobj[i].local_coord.transfer.y = 1;
				tt[i].y *= -1 * ((bl_rand() % 200)/100.0);
			}



			dobj[i].local_coord.transfer.x += tt[i].x;
			dobj[i].local_coord.transfer.y += tt[i].y;

			dobj[i].local_coord.compleate_flg = 0;
			
			
			
			bl3d_sort_object(&dobj[i], &ot);
		}
		
		bl3d_draw_ot(&ot);
	


		wait(1000/30);
	}
}
