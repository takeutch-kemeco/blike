#include <stdlib.h>

#include "drvlfb_build_flag.h"
#include "drvlfb_system.h"

struct DRVLFB_SYSTEM* drvlfb_new_system(
	const char*		fb0_device_name,
	const unsigned int	screen_width,
	const unsigned int	screen_height
)
{
	struct DRVLFB_SYSTEM* a = malloc(sizeof(*a));
	
	a->fb0 = fopen(fb0_device_name, "wb");
	
	a->screen_width  = screen_width;
	a->screen_height = screen_height;
	
	return a;
}

void drvlfb_close_system(struct DRVLFB_SYSTEM* a)
{
	fclose(a->fb0);
	free((void*)a);
}
