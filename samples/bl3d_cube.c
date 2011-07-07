#include "blike.h"

#include "bl3d.h"
#include "bl_load_xpm.h"

static struct BL3D_OT ot;

struct BL3D_VECTOR vertex[8] = {
	{.x = -100,	.y = -100,	.z = -100},
	{.x = +100,	.y = -100,	.z = -100},
	{.x = +100,	.y = +100,	.z = -100},
	{.x = -100,	.y = +100,	.z = -100},

	{.x = -100,	.y = -100,	.z = +100},
	{.x = +100,	.y = -100,	.z = +100},
	{.x = +100,	.y = +100,	.z = +100},
	{.x = -100,	.y = +100,	.z = +100}
};

struct BL3D_VECTOR texture[4] = {
	{.x = 0,	.y = 0},
	{.x = 58,	.y = 0},
	{.x = 58,	.y = 58},
	{.x = 0,	.y = 58}
};

static struct BL3D_TRIANGLE_G_T model_data[12] = {
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},

	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},

	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},

	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},

	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},

	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	},
	{
		.type = BL3D_TRIANGLE_TYPE_G_T,
		.texture_vram = 10,
		.color[0]={1.0,1.0,1.0},
		.color[1]={1.0,1.0,1.0},
		.color[2]={1.0,1.0,1.0},
	}	
};

static void x_init_modex(void)
{
	model_data[0].vertex[0]=vertex[0];
	model_data[0].vertex[1]=vertex[1];
	model_data[0].vertex[2]=vertex[3];
	model_data[0].texture[0]=texture[0];
	model_data[0].texture[1]=texture[1];
	model_data[0].texture[2]=texture[3];

	model_data[1].vertex[0]=vertex[1];
	model_data[1].vertex[1]=vertex[2];
	model_data[1].vertex[2]=vertex[3];
	model_data[1].texture[0]=texture[1];
	model_data[1].texture[1]=texture[2];
	model_data[1].texture[2]=texture[3];

	model_data[2].vertex[0]=vertex[1];
	model_data[2].vertex[1]=vertex[5];
	model_data[2].vertex[2]=vertex[2];
	model_data[2].texture[0]=texture[0];
	model_data[2].texture[1]=texture[1];
	model_data[2].texture[2]=texture[3];

	model_data[3].vertex[0]=vertex[5];
	model_data[3].vertex[1]=vertex[6];
	model_data[3].vertex[2]=vertex[2];
	model_data[3].texture[0]=texture[1];
	model_data[3].texture[1]=texture[2];
	model_data[3].texture[2]=texture[3];

	model_data[4].vertex[0]=vertex[5];
	model_data[4].vertex[1]=vertex[4];
	model_data[4].vertex[2]=vertex[6];
	model_data[4].texture[0]=texture[0];
	model_data[4].texture[1]=texture[1];
	model_data[4].texture[2]=texture[3];

	model_data[5].vertex[0]=vertex[4];
	model_data[5].vertex[1]=vertex[7];
	model_data[5].vertex[2]=vertex[6];
	model_data[5].texture[0]=texture[1];
	model_data[5].texture[1]=texture[2];
	model_data[5].texture[2]=texture[3];

	model_data[6].vertex[0]=vertex[4];
	model_data[6].vertex[1]=vertex[0];
	model_data[6].vertex[2]=vertex[7];
	model_data[6].texture[0]=texture[0];
	model_data[6].texture[1]=texture[1];
	model_data[6].texture[2]=texture[3];

	model_data[7].vertex[0]=vertex[0];
	model_data[7].vertex[1]=vertex[3];
	model_data[7].vertex[2]=vertex[7];
	model_data[7].texture[0]=texture[1];
	model_data[7].texture[1]=texture[2];
	model_data[7].texture[2]=texture[3];

	model_data[8].vertex[0]=vertex[4];
	model_data[8].vertex[1]=vertex[5];
	model_data[8].vertex[2]=vertex[0];
	model_data[8].texture[0]=texture[0];
	model_data[8].texture[1]=texture[1];
	model_data[8].texture[2]=texture[3];

	model_data[9].vertex[0]=vertex[5];
	model_data[9].vertex[1]=vertex[1];
	model_data[9].vertex[2]=vertex[0];
	model_data[9].texture[0]=texture[1];
	model_data[9].texture[1]=texture[2];
	model_data[9].texture[2]=texture[3];

	model_data[10].vertex[0]=vertex[3];
	model_data[10].vertex[1]=vertex[2];
	model_data[10].vertex[2]=vertex[7];
	model_data[10].texture[0]=texture[0];
	model_data[10].texture[1]=texture[1];
	model_data[10].texture[2]=texture[3];

	model_data[11].vertex[0]=vertex[2];
	model_data[11].vertex[1]=vertex[6];
	model_data[11].vertex[2]=vertex[7];
	model_data[11].texture[0]=texture[1];
	model_data[11].texture[1]=texture[2];
	model_data[11].texture[2]=texture[3];
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


#include "test.xpm"

blMain()
{
	openWin(480, 270);
	bl3d_init(480, 270);
	
	
	
	const int vram_width  = 1000;
	const int vram_height = 1000;
	const int vram = 10;
	openVWin(vram, vram_width, vram_height);

	slctWin(vram);			// 描画先の画面を、裏画面１０にする。
	bl_load_xpm(test_xpm, 0, 0);	// 裏画面１０のx,y位置へ、xpm画像をロードする。

	

	x_init_modex();
	bl3d_link_object(&dobj, model_data, 12);

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
			.vector.x=0,	.vector.y=1,	.vector.z=1,
			.color.r=0.3, 	.color.g=0.3,	.color.b=0.3
		},
		{
			.vector.x=1,	.vector.y=1,	.vector.z=-1,
			.color.r=0.29, 	.color.g=0.28,	.color.b=0.27
		},
		{
			.vector.x=-1,	.vector.y=1,	.vector.z=-1,
			.color.r=0.27, 	.color.g=0.28,	.color.b=0.29
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
		case 'j': dobj.local_coord.rotate.y+=0.5;	break;
		case 'k': dobj.local_coord.rotate.y-=0.5;	break;
		case 'h': dobj.local_coord.rotate.x-=0.5;	break;
		case 'l': dobj.local_coord.rotate.x+=0.5;	break;
		case 'm': dobj.local_coord.rotate.z-=0.5;	break;
		case 'u': dobj.local_coord.rotate.z+=0.5;	break;
		}

		dobj.local_coord.rotate.x += 0.01;
		dobj.local_coord.rotate.y += 0.02;
		dobj.local_coord.rotate.z += 0.03;

		dobj.local_coord.compleate_flg = 0;	// 重要！　自分で手動で座標を変更した場合は、座標の再計算の必要があることをbl3dに伝えるために、このフラグをクリアしなければならない。
			
			
			
		bl3d_sort_object(&dobj, &ot);
		
		bl3d_draw_ot(&ot);
	


		wait(1000/30);
	}
}
