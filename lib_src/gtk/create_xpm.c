#include <stdio.h>
#include <glib.h>
#include <glib/gstdio.h>

GString* new_xpm(gint w, gint h)
{
	GString* a = g_string_new(NULL);

	
	g_string_printf(a, "/* XPM */\nstatic char * aaaroundb_xpm[] = {\n\"%d %d 1 1\",\n\".c #000000\",\n", w, h);

	gint j = h;
	while(j-->0){
		gint i = w;
		g_string_append_c(a, '"');
		while(i-->0){
			g_string_append_c(a, '.');
		}
		g_string_append_c(a, '"');
		g_string_append_c(a, ',');
		g_string_append_c(a, '\n');
	}
	g_string_append_c(a, '}');
	g_string_append_c(a, ';');
	g_string_append_c(a, '\n');
	
	
	return a;
}

void create_xpm(const gchar* file_name, gint w, gint h)
{
	GString* a = new_xpm(w, h);
	FILE* fp = g_fopen(file_name, "wb");
	fwrite(a->str, sizeof(gchar), a->len, fp);
	fclose(fp);
}
