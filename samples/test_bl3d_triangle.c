#include "blike.h"

#include "bl3d.h"
#include "bl_load_xpm.h"

static struct BL3D_OT ot;

static struct BL3D_TRIANGLE_G_T model_data[] = {
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.vertex[0]={-325/10,-551/10,0},
		.vertex[1]={325/10,551/10,0},
		.vertex[2]={-325/10,551/10,0},
		.texture[0]={0,0},
		.texture[1]={324,551},
		.texture[2]={0,551},
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.vertex[0]={-325/10,-551/10,0},
		.vertex[1]={325/10,-551/10,0},
		.vertex[2]={325/10,551/10,0},
		.texture[0]={0,0},
		.texture[1]={324,0},
		.texture[2]={324,551},
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
};

static struct BL3D_DOBJ dobj;



void x_fill_gradation(int width, int height, int vram, struct BL3D_CVECTOR* color)
{
	struct BL3D_CVECTOR cur_color = color[0];
	struct BL3D_CVECTOR unit_color = {
		.r = (color[1].r - color[0].r) / height,
		.g = (color[1].g - color[0].g) / height,
		.b = (color[1].b - color[0].b) / height,
	};

	slctWin(vram);

	int i,j;
	for(j = 0; j < height; j++) {
		for(i = 0; i < width; i++) {
			int r = (int)(cur_color.r * 255);
			int g = (int)(cur_color.g * 255);
			int b = (int)(cur_color.b * 255);
			int col =(r<<16) | (g<<8) | (b<<0);
			bl_setPix(i, j, col);
		}
		
		cur_color.r += unit_color.r;
		cur_color.g += unit_color.g;
		cur_color.b += unit_color.b;
	}
}


#include "test_h.xpm"

blMain()
{
	openWin(480, 270);
	bl3d_init(480, 270);
	
	
	
	const int vram_width  = 1000;
	const int vram_height = 1000;
	const int vram = 10;
	openVWin(vram, vram_width, vram_height);

	slctWin(vram);			// 描画先の画面を、裏画面１０にする。
	bl_load_xpm(test_h_xpm, 0, 0);	// 裏画面１０のx,y位置へ、xpm画像をロードする。

	
	
	bl3d_link_object(&dobj, model_data, 2);

	dobj.local_coord.transfer.x = 0;
	dobj.local_coord.transfer.y = 0;
	dobj.local_coord.transfer.z = 1200;
	dobj.local_coord.compleate_flg = 0;
	

	
	struct BL3D_CVECTOR gradation_color[2] = {
		{
			.r = 0.3, .g = 0.4, .b = 0.5
		},
		{
			.r = 1.0, .g = 1.0, .b = 1.0
		}
	};
	x_fill_gradation(480, 270, 4, gradation_color);

	

	struct BL3D_FLAT_LIGHT fl[3] = {
		{
			.vector.x=0,	.vector.y=0,	.vector.z=1,
			.color.r=2, 	.color.g=1,	.color.b=2
		},
		{
			.vector.x=1,	.vector.y=0,	.vector.z=1,
			.color.r=1, 	.color.g=0,	.color.b=0
		},
		{
			.vector.x=0,	.vector.y=1,	.vector.z=1,
			.color.r=0, 	.color.g=0,	.color.b=3
		}
	};
	bl3d_set_flat_light(&fl[0], 0);
	bl3d_set_flat_light(&fl[1], 1);
	bl3d_set_flat_light(&fl[2], 2);

	

	while(1) {
		copyRct0(
			480, 270,
			4, 0, 0,
			0, 0, 0
		);
		
		
		
		bl3d_clear_ot(&ot);
	
		int a = inkey();
		switch(a) {
		case 'j': dobj.local_coord.rotate.y+=0.1;	break;
		case 'k': dobj.local_coord.rotate.y-=0.1;	break;
		case 'h': dobj.local_coord.rotate.x-=0.1;	break;
		case 'l': dobj.local_coord.rotate.x+=0.1;	break;
		case 'm': dobj.local_coord.rotate.z-=0.1;	break;
		case 'u': dobj.local_coord.rotate.z+=0.1;	break;
		}
		
//		dobj.local_coord.rotate.x+=0.01;
//		dobj.local_coord.rotate.y+=0.02;
//		dobj.local_coord.rotate.z+=0.03;

		dobj.local_coord.compleate_flg = 0;
			
			
			
		bl3d_sort_object(&dobj, &ot);
		
		bl3d_draw_ot(&ot);
	


		wait(1000/30);
	}
}
