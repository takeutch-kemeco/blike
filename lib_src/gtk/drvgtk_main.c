#include <gtk/gtk.h>
#include "drvgtk_system.h"

#include "../common/blikedrv.h"


struct BL_WORK			bl_work;
struct DrvGtkPthreadData*	drvgtk_pthread_data; 



int main()
{
	bl_init();
	
	drvgtk_pthread_data = new_DrvGtkPthreadData(
		(gpointer)&bl_work,
		(gint32*)&bl_work.tmcount,
		blMain
	);
	
	run_DrvGtkSystem(drvgtk_pthread_data);
	
//	bl_exit();
}
