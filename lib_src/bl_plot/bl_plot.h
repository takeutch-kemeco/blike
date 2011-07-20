#ifndef __BL_PLOT_H__
#define __BL_PLOT_H__

typedef double (*bl_plot_func)(double x);

/// ２次元のプロッターです。 自作の数学関数などのデバッグに使うことを想定してます。
/// 
/// f: 関数のポインタ。
///	double型の変数 x を受け取り、double型の変数 y を返す関数を渡します。
///	（つまり、テストしたい数学関数などを、この形式にラップして渡してテストします）
///
/// x_max, x_min: f() に渡す x の範囲です。
///	たとえば自作の sin() 関数をテストしたければ、 x_min = -3.14, x_max = +3.14
///	などとして、f()の動作範囲を指定します。
///	（maxima で plot2d() を使う時と似たような感じです。）
///
/// step: x_min から x_max までを、何段階で刻むかを指定します。
///	たとえば自作の sin() 関数をテストするときに、このstepをあまり小さくすると、波がガタガタ
///	になってしまうので、ある程度なめらかに表示させたければ、100程度にすることをおすすめします。
///
/// line_color: 線の色です。 0xRRGGBB形式です。
///
/// bg_color: 背景の罫線や数字の色です。 0xRRGGBB形式です。
///
/// screen_width, screen_height: スクリーンの縦幅、横幅を指定します。
///	普通はウインドウのサイズを指定します。
///
extern void bl_plot(
	bl_plot_func 	f,
	const double	x_min,
	const double	x_max,
	const int	step,
	const int	line_color,
	const int	bg_color,
	double		screen_width,
	double		screen_height
);

/// bl_plot() の、BGを表示しないバージョンです。
/// 引数は bl_color が無い以外は、すべて同じです。
extern void bl_plot_simple(
	bl_plot_func 	f,
	const double	x_min,
	const double	x_max,
	const int	step,
	const int	color,
	double		screen_width,
	double		screen_height
);

#endif // __BL_PLOT_H__
