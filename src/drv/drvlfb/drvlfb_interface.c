#include <string.h>
#include <time.h>

#include "drvlfb_build_flag.h"
#include "drvlfb_system.h"
#include "drvlfb_sleep.h"
#include "drvlfb_malloc.h"

#include "blikedrv.h"
#include "blike0.h"



extern struct BL_WORK __attribute__((aligned(16))) bl_work;



void bld_openWin(int x, int y)
{
	drvlfb_system->screen_width  = x;
	drvlfb_system->screen_height = y;
}



void bld_flshWin(int sx, int sy, int x0, int y0)
{
	const int offset_x =
		(DRVLFB_SYSTEM_FB0_WIDTH - drvlfb_system->screen_width) / 2;
	
	const int offset_y =
		(DRVLFB_SYSTEM_FB0_HEIGHT - drvlfb_system->screen_height) / 2;

	unsigned char* p = (unsigned char*)(bl_work.win[0].buf);
	fseek(
		drvlfb_system->fb0,
		(offset_y * (DRVLFB_SYSTEM_FB0_WIDTH * 4)) + (offset_x * 4),
		SEEK_SET
	);
		
	const int p_inc = drvlfb_system->screen_width * 4;
	const int line_size = drvlfb_system->screen_width * 4;
	const int fb_inc = (DRVLFB_SYSTEM_FB0_WIDTH * 4) - line_size;
	
	int j;
	for(j = 0; j < drvlfb_system->screen_height; j++) {
		fwrite((void*)p, 1, line_size, drvlfb_system->fb0);
		
		fseek(drvlfb_system->fb0, fb_inc, SEEK_CUR);
		p += p_inc;
	}

	fflush(stdout);
}



void bld_flshSys()
{
	fflush(stdout);
}



void bld_waitNF()
{
	drvlfb_msleep(DRVLFB_SYGNAL_CHECK_INTERVAL);
	bl_work.tmcount += DRVLFB_SYGNAL_CHECK_INTERVAL;
}



void bld_exit()
{
	drvlfb_close_system(drvlfb_system);
}



int bld_getSeed()
{
	return (int)time(NULL);
}



void* bld_malloc(unsigned int bytes)
{
	return drvlfb_malloc_aligned16((size_t)bytes);
}

void bld_free(void* p, unsigned int bytes)
{
	drvlfb_free_aligned16((void*)p);
}



extern unsigned char hankaku[4096];
void bld_initFont()
{
	bl_initFont();
	bl_work.mod |= BL_READYFONTS;
	return;
}

int bld_maxfonts()
{
	return 65536 * 4;
}

int bld_vsnprintf(char *b, int n, const char *f, va_list ap)
{
	vsnprintf(b, n, f, ap);
}



void bld_lock()
{
}

void bld_unlock()
{
}



/*
void __bld_get_keybord_state(unsigned long* press, unsigned long* release)
{
	check_and_exit_wt_run_flag();
	
	g_mutex_lock(drvgtk_pthread_data->mutex);
	
	gint i = 8;
	while(i-->0){
		press[i] 	= drvgtk_pthread_data->press->value[i];
		release[i] 	= drvgtk_pthread_data->release->value[i];
	}
	
	next_DrvGtkKeybordState(drvgtk_pthread_data->press, drvgtk_pthread_data->release);
	
	g_mutex_unlock(drvgtk_pthread_data->mutex);
}
*/
