#include <stdio.h>
#include <time.h>
#include <unistd.h>

#include <gtk/gtk.h>
#include "drvgtk_pthread.h"
#include "drvgtk_transrate_keycode.h"

#include "blikedrv.h"
#include "blike0.h"



extern struct BL_WORK			bl_work;
extern struct DrvGtkPthreadData*	drvgtk_pthread_data; 



static void wait_signal_compliate(gboolean* ready)
{
	while((*ready) == TRUE) {
		bld_waitNF();
	}
}



static void bld_showWin(void)
{
	drvgtk_pthread_data->signal->show_window.ready = TRUE;

	wait_signal_compliate(&(drvgtk_pthread_data->signal->show_window.ready));	
}



void bld_openWin(int x, int y)
{
	drvgtk_pthread_data->signal->resize_window.ready  = TRUE;
	drvgtk_pthread_data->signal->resize_window.width  = x;
	drvgtk_pthread_data->signal->resize_window.height = y;

	wait_signal_compliate(&(drvgtk_pthread_data->signal->resize_window.ready));	
}



void bld_flshWin(int sx, int sy, int x0, int y0)
{
	drvgtk_pthread_data->signal->flash_window.ready  		= TRUE;
	drvgtk_pthread_data->signal->flash_window.src_frame_buffer	= (gpointer)(bl_work.win[0].buf);

	wait_signal_compliate(&(drvgtk_pthread_data->signal->flash_window.ready));
}



void bld_flshSys()
{
}



void bld_waitNF()
{
	struct timespec req, rem;
	req.tv_sec  = 0;
	req.tv_nsec = (1000 * 1000) * drvgtk_pthread_data->signal_check_interval;
	
	nanosleep(&req, &rem);
}



void bld_exit()
{
	drvgtk_pthread_data->signal->exit_window.ready = TRUE;

	wait_signal_compliate(&(drvgtk_pthread_data->signal->exit_window.ready));
}






static void __bld_inkey(struct DrvGtkKey* a)
{
	read_c_DrvGtkKeyRingBuffer(drvgtk_pthread_data->key_ring_buffer, a);
}

static void __bld_inkey_view_only(struct DrvGtkKey* a)
{
	read_c_view_only_DrvGtkKeyRingBuffer(drvgtk_pthread_data->key_ring_buffer, a);
}

static void __bld_inkey_BL_WAITKEYNF(void)
{
	struct DrvGtkKey a;
	
	while(1) {
		__bld_inkey_view_only(&a);
		if(a.value != 0) {
			return;
		}
			
		bld_waitNF();
	}
}

static void __bld_inkey_BL_CLEARREP(void)
{
	clean_DrvGtkKeyRingBuffer(drvgtk_pthread_data->key_ring_buffer);
}

int bld_inkey(int flags)
{
	struct DrvGtkKey key;
	
	
	if(flags == 0) {
		__bld_inkey_view_only(&key);
	}
	
	
	if(flags & BL_WAITKEYF) {
		bld_flshWin(
			drvgtk_pthread_data->main_screen->frame_buffer_width,
			drvgtk_pthread_data->main_screen->frame_buffer_width,
			0,
			0
   		);
	}
	
	if(flags & BL_WAITKEYNF) {
		__bld_inkey_BL_WAITKEYNF();
	}
	
	
	if(flags & BL_GETKEY) {
		__bld_inkey(&key);
	}
	
	if(flags & BL_CLEARREP) {
		__bld_inkey_BL_CLEARREP();
	}

	

	switch(key.state) {
	case DrvGtkKeyState_press:
		return transrate_keycode_DrvGtkKey(&key);
		
	case DrvGtkKeyState_release:
		if(flags & BL_DELFFF) {
			return 0;
		}
		else {
			return 0x0FFF;
		}
	}
}






int bld_getSeed()
{
	time_t timer;
	time(&timer);
	return (int) timer;
}



void* bld_malloc(unsigned int bytes)
{
	return (void*)(g_malloc(bytes));
}

void bld_free(void* p, unsigned int bytes)
{
	g_free((gpointer)p);
}



unsigned char hankaku[4096];
//unsigned char* hankaku;
void bld_initFont()
{
	static gboolean font_init_flag = FALSE;
	if(font_init_flag == FALSE) {
//		hankaku = g_malloc(sizeof(*hankaku) * 4096);
		
		FILE* fp = fopen("hankaku.font", "rb");
		fseek(fp, 140, SEEK_SET);
		fread((void*)hankaku, sizeof(unsigned char), 4096, fp);
		fclose(fp);
		
		font_init_flag = TRUE;
	}


	gint i;
	for (i = 0; i < 256; i++) {
 		bl_work.ftyp[i]		= 1;
		bl_work.fptn[i]		= hankaku + 16 * i;
		bl_work.ftyp[i + 256]	= 1;
		bl_work.fptn[i + 256]	= hankaku + 16 * i + 8;
	}

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


