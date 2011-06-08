#include <gtk/gtk.h>
#include "drvgtk_system.h"

#include "../common/blikedrv.h"


struct BL_WORK			bl_work;
struct DrvGtkPthreadData*	drvgtk_pthread_data; 



int main()
{
	gtk_set_locale();
	gtk_init(NULL, NULL);
	gdk_rgb_init();

	bl_init();
	
	drvgtk_pthread_data = new_DrvGtkPthreadData(
		(gpointer)&bl_work,
		(gint32*)&bl_work.tmcount,
		blMain,
		BL_SIZ_KBUF,
		bl_work.kbuf,
		&(bl_work.kbuf_rp),
		&(bl_work.kbuf_wp),
		&(bl_work.kbuf_c)
	);
	
	run_DrvGtkSystem(drvgtk_pthread_data);
	
	bl_exit();
}
