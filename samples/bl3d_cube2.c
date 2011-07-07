#include "blike.h"

#include "bl3d.h"
#include "bl_load_xpm.h"

static struct BL3D_OT ot;

struct BL3D_VECTOR vertex[8] = {
	{.x = -100/5,	.y = -100/5,	.z = -100/5},
	{.x = +100/5,	.y = -100/5,	.z = -100/5},
	{.x = +100/5,	.y = +100/5,	.z = -100/5},
	{.x = -100/5,	.y = +100/5,	.z = -100/5},

	{.x = -100/5,	.y = -100/5,	.z = +100/5},
	{.x = +100/5,	.y = -100/5,	.z = +100/5},
	{.x = +100/5,	.y = +100/5,	.z = +100/5},
	{.x = -100/5,	.y = +100/5,	.z = +100/5}
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

static void x_init_model(void)
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

static struct BL3D_DOBJ dobj[1000];
static struct BL3D_VECTOR tt[1000];
static struct BL3D_VECTOR tr[1000];
static const float G=3;


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



struct BL3D_VECTOR* bl3d_vector_to_rotate(
	struct BL3D_VECTOR* rotate,
	struct BL3D_VECTOR* vector
)
{
	rotate->x = -bl3d_atan2(vector->y, vector->z);
	rotate->y = bl3d_atan2(vector->x, vector->z);
	rotate->z = 0;
	
	return rotate;
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

	

	x_init_model();
	
	int dobj_len = 500;
	int i;
	for(i=0; i<dobj_len; i++) {
		bl3d_link_object(&dobj[i], model_data, 12);
		tt[i].x = ((bl_rand() % 1000) - 500) * 0.01;
		tt[i].y = ((bl_rand() % 1000) - 500) * 0.01;
		tt[i].z = ((bl_rand() % 1000) - 500) * 0.05;

		tr[i].x = ((bl_rand() % 1000) - 500) * 0.0001;
		tr[i].y = ((bl_rand() % 1000) - 500) * 0.0001;
		tr[i].z = ((bl_rand() % 1000) - 500) * 0.0001;

		dobj[i].local_coord.transfer.x = 0 + ((bl_rand() % 2000) - 1000);
		dobj[i].local_coord.transfer.y = 0 + ((bl_rand() % 1000) - 500);
		dobj[i].local_coord.transfer.z = 10000 + ((bl_rand() % 2000) - 1000);
		dobj[i].local_coord.compleate_flg = 0;
	}
	
	dobj_len = 5;


	
	struct BL3D_CVECTOR gradation_color[2] = {
		{
			.r = 0.4, .g = 0.5, .b = 0.6
		},
		{
			.r = 1.0, .g = 1.0, .b = 1.0
		}
	};
	x_fill_gradation(480, 270, 4, gradation_color);

	

	struct BL3D_FLAT_LIGHT fl[3] = {
		{
			.vector.x=0,	.vector.y=0,	.vector.z=1,
			.color.r=0.55, 	.color.g=0.54,	.color.b=0.53
		},
		{
			.vector.x=1,	.vector.y=1,	.vector.z=-1,
			.color.r=0.55, 	.color.g=0.54,	.color.b=0.53
		},
		{
			.vector.x=-1,	.vector.y=1,	.vector.z=-1,
			.color.r=0.55, 	.color.g=0.54,	.color.b=0.53
		}
	};
	
	
	
	struct BL3D_VIEW view[2];
	view[0].rotate = bl3d_0_vector;
	view[0].rotate.z = -3.14/4;
	view[0].transfer = bl3d_0_vector;
	bl3d_init_coordinate(&view[0].local_coord, NULL);

	view[1].rotate = bl3d_0_vector;
	view[1].transfer = bl3d_0_vector;
	bl3d_init_coordinate(&view[1].local_coord, NULL);

	int view_mode = 0;
	
	
	while(1) {
		copyRct0(
			480, 270,
			4, 0, 0,
			0, 0, 0
		);

		bl_locate(0,0);
		bl_printf("jk: view down, up\n");
		bl_printf("hl: view left, right\n");
		bl_printf("um: view z+, z-\n\n");
		bl_printf("JK: view rotY +, -\n");
		bl_printf("HL: view rotX -, +\n");
		bl_printf("UM: view rotZ -, +\n");
		bl_printf("r: view reset\n\n");
		bl_printf("1: normal view\n");
		bl_printf("2: trace view\n\n");
		bl_printf("i: inc cube\n");
		bl_printf("d: dec cube\n\n");
		bl_printf("num cube[%03d]\n", dobj_len);

	
	
		int a = inkey();
		switch(a) {
		case 'j': view[view_mode].transfer.y+=100.5;	break;
		case 'k': view[view_mode].transfer.y-=100.5;	break;
		case 'h': view[view_mode].transfer.x-=100.5;	break;
		case 'l': view[view_mode].transfer.x+=100.5;	break;
		case 'm': view[view_mode].transfer.z-=100.5;	break;
		case 'u': view[view_mode].transfer.z+=100.5;	break;

		case 'J': view[view_mode].rotate.y+=100.5;	break;
		case 'K': view[view_mode].rotate.y-=100.5;	break;
		case 'H': view[view_mode].rotate.x-=100.5;	break;
		case 'L': view[view_mode].rotate.x+=100.5;	break;
		case 'M': view[view_mode].rotate.z-=100.5;	break;
		case 'U': view[view_mode].rotate.z+=100.5;	break;
		
		case 'r':
			view[view_mode].rotate   = bl3d_0_vector;
			view[view_mode].rotate.z = -3.14/4;
			view[view_mode].transfer = bl3d_0_vector;
			break;
		
		case '1': view_mode=0;	break;
		case '2': view_mode=1;	break;

		case 'i': dobj_len = (dobj_len<300)? dobj_len+1: 300;	break;
		case 'd': dobj_len = (dobj_len>0  )? dobj_len-1: 0;	break;
		}
		
		
		
		{
			struct BL3D_MATRIX lw;
			bl3d_get_lw(&lw, &dobj[0].local_coord);
			
			struct BL3D_VECTOR t;
			t.x = lw.t[0] - view[1].transfer.x;
			t.y = lw.t[1] - view[1].transfer.y;
			t.z = lw.t[2] - view[1].transfer.z;
			
			bl3d_vector_to_rotate(&view[1].rotate, &t);
		}
		


		bl3d_set_view(&view[view_mode]);
		
		bl3d_set_flat_light(&fl[0], 0);
		bl3d_set_flat_light(&fl[1], 1);
		bl3d_set_flat_light(&fl[2], 2);
		
		
		
		bl3d_clear_ot(&ot);
		
		

		static int G_mode = 4;
		static int G_counter = 0;
		
		if(G_counter++ > 100) {
			G_counter = 0;
			G_mode<<=1;
			if(G_mode > 32) {G_mode = 1;}
		}

		for(i = 0; i < dobj_len; i++) {
			switch(G_mode) {
			case 1:  tt[i].x += G; break;
			case 2:  tt[i].x -= G; break;
			case 4:  tt[i].y += G; break;
			case 8:  tt[i].y -= G; break;
			case 16: tt[i].z += G; break;
			case 32: tt[i].z -= G; break;
			}
			
			
			
			dobj[i].local_coord.transfer.x += tt[i].x;
			dobj[i].local_coord.transfer.y += tt[i].y;
			dobj[i].local_coord.transfer.z += tt[i].z;
			
			if(dobj[i].local_coord.transfer.x >= +3000 || dobj[i].local_coord.transfer.x <= -3000) {
				tt[i].x = -tt[i].x;
				dobj[i].local_coord.transfer.x += tt[i].x * 2;
				switch(G_mode){case 1:case 2: tt[i].x *= 0.6; break;}
			}

			if(dobj[i].local_coord.transfer.y >= 1000 || dobj[i].local_coord.transfer.y <= -1000) {
				tt[i].y = -tt[i].y;
				dobj[i].local_coord.transfer.y += tt[i].y * 2;
				switch(G_mode){case 4:case 8: tt[i].y *= 0.5; break;}
			}

			if(dobj[i].local_coord.transfer.z >= +31000 || dobj[i].local_coord.transfer.z <= +3000) {
				tt[i].z = -tt[i].z;
				dobj[i].local_coord.transfer.z += tt[i].z * 2;
				switch(G_mode){case 16:case 32: tt[i].z *= 0.7; break;}
			}

			
			dobj[i].local_coord.rotate.x += tr[i].x;
			dobj[i].local_coord.rotate.y += tr[i].y;
			dobj[i].local_coord.rotate.z += tr[i].z;

			dobj[i].local_coord.compleate_flg = 0;	// 重要！　自分で手動で座標を変更した場合は、座標の再計算の必要があることをbl3dに伝えるために、このフラグをクリアしなければならない。
		
			
			
			bl3d_sort_object(&dobj[i], &ot);
		}
		
		bl3d_draw_ot(&ot);
	


		wait(1000/60);
	}
}
