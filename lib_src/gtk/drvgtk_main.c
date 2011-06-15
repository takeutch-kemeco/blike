#include <gtk/gtk.h>
#include "drvgtk_system.h"

#include "../common/blikedrv.h"


struct BL_WORK			bl_work;
struct DrvGtkPthreadData*	drvgtk_pthread_data; 



extern void bl_init();
extern void bl_exit();

int main()
{
	g_thread_init (NULL);
	
	gdk_threads_init();
	gdk_rgb_init();
	
	gtk_init(NULL, NULL);
	gtk_set_locale();
	
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
		&(bl_work.kbuf_c)
	);
	
	run_DrvGtkSystem(drvgtk_pthread_data);
}
