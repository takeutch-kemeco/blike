#include <pthread.h>
#include <glib.h>
#include "main_window.h"
#include "main_screen.h"
#include "drvgtk_signal.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keybord_state.h"

#ifndef __DRVGTK_PTHREAD_H__
#define __DRVGTK_PTHREAD_H__

#define DRVGTK_SYGNAL_CHECK_INTERVAL	(1000/250)	/* 250Hz */

struct DrvGtkPthreadData {
	struct MainWindow*	main_window;
	struct MainScreen*	main_screen;

	
	gpointer		shared_data;
	gint32*			time_count;
	
	
	struct DrvGtkSignal*	signal;
	
	GMutex*			mutex;
	
	gboolean (*window_update_program)(gpointer data);
	int (*control_program)();
	void (*init_control_program)();
	void (*close_control_program)();
	
	GThread*		ptid;
	gboolean		wt_run_flag;
	
	
	struct DrvGtkKeyRingBuffer*	key_ring_buffer;
	
	struct DrvGtkKeybordState*	press;
	struct DrvGtkKeybordState*	release;
	struct DrvGtkKeybordState*	key_transform_table;
} __attribute__((aligned(16)));

extern void pthread_main(struct DrvGtkPthreadData* data);

#endif // __DRVGTK_PTHREAD_H__
