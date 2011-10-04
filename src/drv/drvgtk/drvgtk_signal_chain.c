#include <glib.h>
#include "drvgtk_build_flag.h"

#include "drvgtk_build_flag.h"
#include "drvgtk_pthread.h"
#include "drvgtk_signal_chain.h"



static void resize_window(struct DrvGtkPthreadData* a, gint width, gint height)
{
	resize_MainWindow(a->main_window, width, height);
	resize_MainScreen(a->main_screen, width, height, a->main_window);

	show_MainWindow(a->main_window);
	redraw_MainScreen(a->main_screen);
}



static void show_window(struct DrvGtkPthreadData* a)
{
	show_MainWindow(a->main_window);
}



static inline void disable_sse_flash_window(guchar* dst, guint32* src, gint i)
{
	while(i-->0){
		*dst++ = ((*src)>>16) & 0xFF;
		*dst++ = ((*src)>>8 ) & 0xFF;
		*dst++ = ((*src)>>0 ) & 0xFF;
		
		src++;
	}
}

static inline void enable_sse_flash_window(guchar* dst, guint32* src, gint _i)
{
	gint ii = (_i % 4) + 2;
	gint i = (_i - ii) / 4;
	
	const guint32 __attribute__((aligned(16)))mskR[4] = {0xFF,     0xFF,     0xFF,     0xFF}; 
	const guint32 __attribute__((aligned(16)))mskG[4] = {0xFF00,   0xFF00,   0xFF00,   0xFF00}; 
	const guint32 __attribute__((aligned(16)))mskB[4] = {0xFF0000, 0xFF0000, 0xFF0000, 0xFF0000};

	guint8 __attribute__((aligned(16)))tmp[16];
	
	
	__asm__ volatile(
		"movdqa (%0), %%xmm5;"
		"movdqa (%1), %%xmm6;"
		"movdqa (%2), %%xmm7;"
		:
		:"r"(mskR), "r"(mskG), "r"(mskB)
	);

	
	while(i-->0){
		__asm__ volatile(
			"prefetchnta 1024(%0);"
			
			"movdqa (%0),   %%xmm1;"
			"movdqa %%xmm1, %%xmm0;"
			"movdqa %%xmm1, %%xmm2;"

			"psrld $16, %%xmm0;"
			"pslld $16, %%xmm2;"

			"andpd %%xmm5, %%xmm0;"
			"andpd %%xmm6, %%xmm1;"
			"andpd %%xmm7, %%xmm2;"

			"orpd   %%xmm2, %%xmm1;"
			"orpd   %%xmm1, %%xmm0;"

			"movdqa %%xmm0, (%2);"
			"movl (%2), %%eax;"
			"movl %%eax, (%1);"
			"movl 4(%2), %%eax;"
			"movl %%eax, 3(%1);"
			"movl 8(%2), %%eax;"
			"movl %%eax, 6(%1);"
			"movl 12(%2), %%eax;"
			"movl %%eax, 9(%1);"
			:
			:"r"(src), "r"(dst), "r"(tmp)
			:"%eax", "memory"
		);
		
		src += 4;
		dst += 12;
	}
	

	disable_sse_flash_window(dst, src, ii);
}

static void flash_window(struct DrvGtkPthreadData* a, gpointer src_frame_buffer)
{
	guint32* src = (guint32*)src_frame_buffer;
	guchar*  dst = a->main_screen->frame_buffer;

	gint i = a->main_screen->frame_buffer_width * a->main_screen->frame_buffer_height;
	if(src != NULL) {
#ifdef __ENABLE_SSE2__
		enable_sse_flash_window(dst, src, i);
#else
		disable_sse_flash_window(dst, src, i);
#endif // __ENABLE_SSE2__
	}


	redraw_MainScreen(a->main_screen);
}



static void exit_window(struct DrvGtkPthreadData* a)
{
	gtk_main_quit();
}



gboolean update_DrvGtkSignalChain(gpointer data)
{
	struct DrvGtkPthreadData* a = (struct DrvGtkPthreadData*)data;
	

	(*(a->time_count)) += DRVGTK_SYGNAL_CHECK_INTERVAL;
	
	
	if(a->signal->resize_window.ready == TRUE) {
		resize_window(a, a->signal->resize_window.width, a->signal->resize_window.height);
		a->signal->resize_window.ready = FALSE;
	}

	if(a->signal->show_window.ready == TRUE) {
		show_window(a);
		a->signal->show_window.ready = FALSE;
	}
	
	if(a->signal->flash_window.ready == TRUE) {
		flash_window(a, a->signal->flash_window.src_frame_buffer);
		a->signal->flash_window.ready = FALSE;
	}
	
	if(a->signal->exit_window.ready == TRUE) {
		exit_window(a);
		a->signal->exit_window.ready == FALSE;
	}
	
	
	return TRUE;
}


