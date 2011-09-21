#include "blike.h"
#include "bl_complex.h"

blMain()
{
	struct BL_COMPLEX a = {2,3};
	struct BL_COMPLEX b = a;

	bl_print_complex(&a); bl_putc('\n');
	bl_add_complex(&a, &b);
	bl_print_complex(&a); bl_putc('\n');
	bl_sub_complex(&a, &b);
	bl_print_complex(&a); bl_putc('\n');
	bl_mul_complex(&a, &b);
	bl_print_complex(&a); bl_putc('\n');
	bl_div_complex(&a, &b);
	bl_print_complex(&a); bl_putc('\n');
	
	b.real = 1; b.image = 3.141592653589793;
	a.real = 3; a.image = 0;
	bl_pow_complex(&a, &b);
	bl_print_complex(&a); bl_putc('\n');
	
	b.real = 2; b.image = 3.141592653589793;
	a.real = 3; a.image = 4;
	bl_pow_complex(&a, &b);
	bl_print_complex(&a); bl_putc('\n');

	wait(-1);
}

