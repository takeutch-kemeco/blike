#include <math.h>
#include "bl_complex.h"

#define BL_PI 3.141592653589793
#define BL_E  2.718281828459045

static void bl_dft(
	struct BL_COMPLEX* dst,
	const unsigned int index, 
	struct BL_COMPLEX* src,
	const unsigned int N
)
{
	struct BL_COMPLEX t = {0, 0};

	int i;
	for(i = 0; i < N; i++) {
		struct BL_COMPLEX tmp = src[i];
		struct BL_COMPLEX w = {0, -((2 * BL_PI)/N) * index * i};
		struct BL_COMPLEX e = {BL_E, 0};

		bl_pow_complex(&e, &w);
		bl_mul_complex(&tmp, &e);
		bl_add_complex(&t, &tmp);
	}

	dst[index] = t;
}

static void bl_idft(
	struct BL_COMPLEX* dst,
	const unsigned int index, 
	struct BL_COMPLEX* src,
	const unsigned int N
)
{
	struct BL_COMPLEX t = {0, 0};

	int i;
	for(i = 0; i < N; i++) {
		struct BL_COMPLEX tmp = src[i];
		struct BL_COMPLEX w = {0, +((2 * BL_PI)/N) * index * i};
		struct BL_COMPLEX e = {BL_E, 0};

		bl_pow_complex(&e, &w);
		bl_mul_complex(&tmp, &e);
		bl_add_complex(&t, &tmp);
	}

	struct BL_COMPLEX A = {1.0 / N, 0};
	bl_mul_complex(&t, &A);

	dst[index] = t;
}

void bl_dft_complex(
	struct BL_COMPLEX* dst,
	struct BL_COMPLEX* src,
	const unsigned int N
)
{
	int i;
	for(i = 0; i < N; i++) {
		bl_dft(dst, i, src, N);
	}
}

void bl_idft_complex(
	struct BL_COMPLEX* dst,
	struct BL_COMPLEX* src,
	const unsigned int N
)
{
	int i;
	for(i = 0; i < N; i++) {
		bl_idft(dst, i, src, N);
	}	
}

