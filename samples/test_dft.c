#include "blike.h" 
#include "bl_complex.h"
#include "bl_dft.h"

void bl_print_complex_list(struct BL_COMPLEX* a, int n)
{
	int i;
	for(i = 0; i < n; i++) {
		bl_print_complex(&a[i]);
		bl_putc('\n');
	}
}

blMain()
{
	bl_openWin(480, 960);

	struct BL_COMPLEX src[8] = {
		0.0, 0,
		0.4, 0,
		0.5, 0,
		0.7, 0,
		0.9, 0,
		0.4, 0,
		0.2, 0,
		0.1, 0,
	};

	bl_printf("<original data>\n");
	bl_print_complex_list(src, 8);
	bl_putc('\n');

	bl_printf("<dft>\n");
	struct BL_COMPLEX dst[8];
	bl_dft_complex(dst, src, 8);
	bl_print_complex_list(dst, 8);
	bl_putc('\n');

	bl_printf("<idft>\n");
	struct BL_COMPLEX dst2[8];
	bl_idft_complex(dst2, dst, 8);
	bl_print_complex_list(dst2, 8);

	wait(-1);
}

