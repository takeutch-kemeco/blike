#include <math.h>
#include "bl_complex.h"

#define BL_PI 3.141592653589793
#define BL_E  2.718281828459045

extern void bl_printf(const char *s, ...);

void bl_print_complex(struct BL_COMPLEX* a)
{
	bl_printf("[%f + %fi]", a->real, a->image);
}

void bl_add_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src)
{
	dst->real  += src->real;
	dst->image += src->image;
}

void bl_sub_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src)
{
	dst->real  -= src->real;
	dst->image -= src->image;
}

static double bl_get_rot_complex(struct BL_COMPLEX* a)
{
	if(a->image == 0) {
		return 0;
	}
	else if(a->real > 0) {
		return atan(a->image / a->real);
	}
	else if(a->real == 0 && a->image > 0) {
		return BL_PI / 2;
	}
	else if(a->real == 0 && a->image < 0) {
		return 3 * (BL_PI / 2);
	}
	else if(a->real < 0) {
		return atan(a->image / a->real) + BL_PI;
	}
	else {
		bl_printf("err : bl_get_rot_complex()\n");
		return 0;	
	}
}

void bl_mul_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src)
{
	struct BL_COMPLEX tmp = *dst;
	dst->real  = (tmp.real * src->real) - (tmp.image * src->image);
	dst->image = (tmp.real * src->image) + (tmp.image * src->real);
}

void bl_div_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src)
{
	struct BL_COMPLEX tmp = *dst;
	const double base = (src->real * src->real) + (src->image * src->image);
	dst->real  = ((tmp.real  * src->real) + (tmp.image * src->image)) / base;
	dst->image = ((tmp.image * src->real) - (tmp.real  * src->image)) / base;
}

void bl_pow_complex(struct BL_COMPLEX* x, struct BL_COMPLEX* y)
{
	double xr = sqrt(x->real * x->real + x->image * x->image);
	double xrot = bl_get_rot_complex(x);
	double P = pow(xr, y->real);
	double L = log(xr);
	double R = y->image * L + y->real * xrot;
	double W = pow(BL_E, -BL_PI * xrot);
	double U = P * W;
	x->real  = U * cos(R);
	x->image = U * sin(R);
}

