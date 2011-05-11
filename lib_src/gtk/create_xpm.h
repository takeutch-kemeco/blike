#include <glib.h>

#ifndef __CREATE_XPM_H__
#define __CREATE_XPM_H__

extern GString* new_xpm(gint w, gint h);
extern void create_xpm(const gchar* file_name, gint w, gint h);

#endif //__CREATE_XPM_H__
