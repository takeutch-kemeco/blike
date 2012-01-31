#include <math.h>
#include <bl_fast_sincos.h>

#ifndef __BL3D_MATH_MACRO_H__
#define __BL3D_MATH_MACRO_H__

///逆数を得る
///
/// dst: float*
/// src: float*
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_RCP(dst, src) {					\
	__asm__ volatile(					\
		"rcpss   (%0),   %%xmm0;"			\
		"movss   %%xmm0, (%1);"				\
		:						\
		:"r"((src)), "r"((dst))				\
		:"memory"					\
	);							\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_RCP(dst, src) {					\
	*(dst) = 1.0 / *(src);					\
}
#endif // __ENABLE_SSE3__

///平方根を得る
///
/// dst: float*
/// src: float*
///
// SSE3 を使用可能な場合
#ifdef __ENABLE_SSE3__
#define BL3D_SQRT(dst, src) {					\
	__asm__ volatile(					\
		"rsqrtss (%0),   %%xmm0;"			\
		"mulss   (%0),   %%xmm0;"			\
		"movss   %%xmm0, (%1);"				\
		:						\
		:"r"((src)), "r"((dst))				\
		:"memory"					\
	);							\
}
// SSE3 を使用不可能な場合
#else
#define BL3D_SQRT(dst, src) {					\
	*(dst) = sqrtf(*(src));					\
}
#endif // __ENABLE_SSE3__

#define BL3D_ATAN2(y, x) (atan2f((y),(x)))

#endif // __BL3D_MATH_MACRO_H__
