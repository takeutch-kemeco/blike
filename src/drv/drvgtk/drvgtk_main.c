#include <gtk/gtk.h>
#include "config.h"

#include "drvgtk_system.h"
#include "drvgtk_language.h"

#include "blikedrv.h"


struct BL_WORK __attribute__((aligned(16))) bl_work;

struct DrvGtkPthreadData* drvgtk_pthread_data; 

int bl_argc;
char** bl_argv;


extern void bl_init();
extern void bl_exit();

int main(int argc, char** argv)
{
	bl_argc = argc;
	bl_argv = argv;

	gdk_threads_init();
	
	gtk_init(NULL, NULL);
	
	
	drvgtk_pthread_data = new_DrvGtkPthreadData(
		(gpointer)&bl_work,
		(gint32*)&bl_work.tmcount,
		blMain,
		bl_init,
		bl_exit,
		BL_SIZ_KBUF,
		bl_work.kbuf,
		&(bl_work.kbuf_rp),
		&(bl_work.kbuf_wp),
		&(bl_work.kbuf_c),
		DRVGTK_KEYBORD_STATE_LANGUAGE_JA
	);
	
	run_DrvGtkSystem(drvgtk_pthread_data);
	
	free_DrvGtkPthreadData(drvgtk_pthread_data);
	
	return 0;
}
