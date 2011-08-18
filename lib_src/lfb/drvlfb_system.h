#include <stdio.h>

#ifndef __DRVLFB_SYSTEM_H__
#define __DRVLFB_SYSTEM_H__

#define DRVLFB_SYGNAL_CHECK_INTERVAL (1000 / 250)

struct DRVLFB_SYSTEM {
	FILE*		fb0;
	unsigned int	screen_width;
	unsigned int	screen_height;
} __attribute__((aligned(16)));

extern struct DRVLFB_SYSTEM* drvlfb_system;

extern struct DRVLFB_SYSTEM* drvlfb_new_system(
	const char*		fb0_device_name,
	const unsigned int	screen_width,
	const unsigned int	screen_height
);

extern void drvlfb_close_system(struct DRVLFB_SYSTEM* a);

#endif // __DRVLFB_SYSTEM_H__
