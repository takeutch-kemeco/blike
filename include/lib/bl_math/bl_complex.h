#ifndef __BL_COMPLEX_H__
#define __BL_COMPLEX_H__

struct BL_COMPLEX {
	double real;
	double image;
};

extern void bl_print_complex(struct BL_COMPLEX* a);
extern void bl_add_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src);
extern void bl_sub_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src);
extern void bl_mul_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src);
extern void bl_div_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src);
extern void bl_pow_complex(struct BL_COMPLEX* x, struct BL_COMPLEX* y);

#endif // __BL_COMPLEX_H_

