#include <gtk/gtk.h>
#include "drvgtk_pthread.h"

#ifndef __DRVGTK_SYSTEM_H__
#define __DEVGTK_SYSTEM_H__

extern struct DrvGtkPthreadData* new_DrvGtkPthreadData(
	gpointer	shared_data,
	gint32* 	time_count,
	int 		(*control_program)()
);

extern void run_DrvGtkSystem(struct DrvGtkPthreadData* a);

#endif // __DRVGTK_SYSTEM_H__
