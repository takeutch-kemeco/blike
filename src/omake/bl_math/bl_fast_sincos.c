#define SCTBL_NUM ((unsigned int)(1<<14))
#include "sctbl.h"

#define BL_PI 3.141592653589793

void bl_fast_sincos(float* s, float* c, float a)
{
	static const double u = ((double)SCTBL_NUM) / (2 * BL_PI);
	unsigned int i = ((unsigned int)(u * a)) & (SCTBL_NUM - 1);
	*s = sctbl[i][0];
	*c = sctbl[i][1];
}
