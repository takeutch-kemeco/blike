#include "bl_complex.h"

#ifndef __BL_DFT_H__
#define __BL_DFT_H__

extern void bl_dft_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src, const unsigned int N);
extern void bl_idft_complex(struct BL_COMPLEX* dst, struct BL_COMPLEX* src, const unsigned int N
);

#endif // __BL_DFT_H__

