#include "blike.h"
#include "bl_plot.h"

static void bl_plot_get_y_minmax(
	double*		y_min,
	double*		y_max,
	bl_plot_func 	f,
	const double	x_min,
	const double	x_max,
	const int	step
)
{
	*y_min = 1e+255;
	*y_max = 1e-255;
	
	
	const double x_unit = (x_max - x_min) / step;
	double x = x_min;

	int i;
	for(i = 0; i < step; i++) {
		double y = f(x);
		
		if(y < *y_min) {
			*y_min = y; 
		}
		
		if(y > *y_max) {
			*y_max = y; 
		}
		
		x += x_unit;
	}
}

static double bl_plot_fabs(double a)
{
	return sqrt(a * a);
}

void bl_plot_simple(
	bl_plot_func 	f,
	const double	x_min,
	const double	x_max,
	const int	step,
	const int	color,
	double		screen_width,
	double		screen_height
)
{
	const double screen_center_x = screen_width  / 2;
	const double screen_center_y = screen_height / 2;

	screen_width  -= 150;
	screen_height -= 150;

	double screen_x = screen_center_x - (screen_width  / 2);
	double screen_y = screen_center_y + (screen_height / 2);
	
	const double screen_x_unit = screen_width  / step;
	
	
	
	double y_min;
	double y_max;
	bl_plot_get_y_minmax(&y_min, &y_max, f, x_min, x_max, step);
	
	double y_scale = screen_height / (y_max - y_min);
	
	
	
	const double x_unit = (x_max - x_min) / step;
	double x = x_min;
	
	double draw_x;
	double draw_y;
	double old_x;
	double old_y;
	
	int i;
	for(i = 0; i <= step; i++) {
		double y = f(x);
		y = (y - y_min) * y_scale;
		
		draw_x = screen_x;
		draw_y = screen_y - y;
		
		if(i >= 1) {
			bl_setCol(color);
			bl_drawLine(old_x, old_y, draw_x, draw_y);
		}
		
		old_x = draw_x;
		old_y = draw_y;
		
		x += x_unit;
		screen_x += screen_x_unit;
	}
}

static void bl_plot_bg(
	bl_plot_func 	f,
	const double	x_min,
	const double	x_max,
	const int	step,
	const int	color,
	double		screen_width,
	double		screen_height
)
{
	const double screen_center_x = screen_width  / 2;
	const double screen_center_y = screen_height / 2;

	screen_width  -= 150;
	screen_height -= 150;

	double screen_x = screen_center_x - (screen_width  / 2);
	double screen_y = screen_center_y;
	
	const double screen_x_unit = screen_width  / step;
	const double screen_y_unit = screen_height / step;
	
	bl_setCol(color);
	
	int i;
	for(i = 0; i <= (step / 2); i++) {
		bl_drawLine(
			screen_x,
			screen_y + (screen_y_unit * i),
			screen_x + screen_width,
			screen_y + (screen_y_unit * i)
		);
		
		bl_drawLine(
			screen_x,
			screen_y - (screen_y_unit * i),
			screen_x + screen_width,
			screen_y - (screen_y_unit * i)
		);
	}
	
	for(i = 0; i <= step; i++) {
		bl_drawLine(
			screen_x + (screen_x_unit * i),
			screen_y - (screen_height / 2),
			screen_x + (screen_x_unit * i),
			screen_y + (screen_height / 2)
		);
	}

	
	
	
	double y_min;
	double y_max;
	bl_plot_get_y_minmax(&y_min, &y_max, f, x_min, x_max, step);
	
	double y_unit = (y_max - y_min) / step;
	
	for(i = 0; i <= step; i++) {
		bl_drawStr(
			screen_x - 60,
			screen_y + (screen_height / 2) - (screen_y_unit * i) - 8,
			1,
			1,
			"%+.4f",
			y_min + (y_unit * i)
		);
	}
	
	
	
	const double x_unit = (x_max - x_min) / step;
	
	for(i = 0; i <= step; i++) {
		if((i % 2) == 0) {
			bl_drawStr(
				screen_x + (screen_x_unit * i) - 8,
				screen_y + (screen_height / 2) + 4,
				1,
				1,
				"%+.4f",
				x_min + (x_unit * i)
			);
		}
		else {
			bl_drawStr(
				screen_x + (screen_x_unit * i) - 8,
				screen_y + (screen_height / 2) + 4 + 16,
				1,
				1,
				"%+.4f",
				x_min + (x_unit * i)
			);
		}
	}
}

void bl_plot(
	bl_plot_func 	f,
	const double	x_min,
	const double	x_max,
	const int	step,
	const int	line_color,
	const int	bg_color,
	double		screen_width,
	double		screen_height
)
{
	int bg_step = screen_width / 64;
	bl_plot_bg(f, x_min, x_max, bg_step, bg_color, screen_width, screen_height);
	bl_plot_simple(f, x_min, x_max, step, line_color, screen_width, screen_height);
}
