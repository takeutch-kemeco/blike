#include "blike.h"
#include "bl_plot.h"

static const double e = 2.71828; 


/// 式の係数
static double a = 1.0;


/// テストしたい関数
/// （double型を受け取り、計算し、double型を返す。　という形式の関数であること）
double aaa(double x) {return pow(e, -(1*a * x*x));}

double bbb(double x) {return pow(e, -(2*a * x*x));}

double ccc(double x) {return pow(e, -(3*a * x*x));}

double ddd(double x) {return pow(e, -(4*a * x*x));}



blMain()
{
	const int screen_width  = 640;
	const int screen_height = 480; 
	bl_openWin(screen_width, screen_height);
	
	while(1) {
		// 上下キーで、係数を変化させて遊ぼう
		int key = bl_inkey1();
		switch(key) {
		case KEY_UP:	a += 0.01;		break;
		case KEY_DOWN:	a -= 0.01;		break;
		}
		
		
		
		
		
		// 画面クリア
		bl_setBCol(0x001122);
		bl_cls();
		
		
		// 最初のbl_plot()はBG描画付き。　以降の_simple()はBG描画無し。
		// 引数は、
		//	関数ポインタ、
		//	ｘの範囲が-3.14 ~ +3.14、
		//	xの範囲を 100 刻み
		//	線の色、BGの色、
		//	画面のタテヨコの幅
		bl_plot       (aaa, -3.14, +3.14, 100, 0xFFAAAA, 0x445566, 640,400);
		
		bl_plot_simple(bbb, -3.14, +3.14, 100, 0xAAFFAA,           640,400);
		
		bl_plot_simple(ccc, -3.14, +3.14, 100, 0xAAAAFF,           640,400);
		
		bl_plot_simple(ddd, -3.14, +3.14, 100, 0xAAFFFF,           640,400);
		
		
		
		// 1/60秒毎に画面更新
		wait(1000/60);
	}
}
