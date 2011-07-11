#include <gtk/gtk.h>
#include "drvgtk_pthread.h"

#ifndef __DRVGTK_SYSTEM_H__
#define __DEVGTK_SYSTEM_H__

extern struct DrvGtkPthreadData* new_DrvGtkPthreadData(
	gpointer	shared_data,
	gint32* 	time_count,
	int 		(*control_program)(),
	void		(*init_control_program)(),
	void		(*close_control_program)(),
	gint32		key_len,
	gint32*		int_key,
	gint32*		read_index,
	gint32*		write_index,
	gint32*		key_count,
	gint		language
);

extern free_DrvGtkPthreadData(struct DrvGtkPthreadData* a);

extern void run_DrvGtkSystem(struct DrvGtkPthreadData* a);

#endif // __DRVGTK_SYSTEM_H__
