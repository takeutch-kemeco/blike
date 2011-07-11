//#define __DEBUG__

#include <glib.h>

gpointer drvgtk_malloc_aligned16(gsize size)
{
	guint8* org = g_malloc(size + 32);
	guint32 org_address = (guint32)org;
	guint32 diff_address = org_address % 16;

#ifdef __DEBUG__
	g_printf("org[%p], org_address[0x%x], diff_address[[%d]\n", org, org_address, diff_address);
#endif // __DEBUG__
	
	
	guint32* header = (guint32*)(org + diff_address);
	*header = org_address;
	
	guint32* a = header + 4;        // header + 16byte

#ifdef __DEBUG__
	g_printf(
		"header[%p], *header[0x%x], ret_address[%p], *ret_address[%d], ret_address%16[%d]\n\n", 
		header,
		*header,
		a,
		*a,
		(*a) % 16
	);
#endif // __DEBUG__
	
	
	if((*a % 16)!=0) {
		g_printf("err: drvgtk_malloc_aligned16()\n");
	}

	return (gpointer)a;
}

void drvgtk_free_aligned16(gpointer a)
{       
	guint32* header = ((guint32*)a) - 4;
	guint32 org_address = *header;  
	gpointer org = (guint32*)org_address;

#ifdef __DEBUG__
	g_printf("header[%p], org_address[%d], org=[%p]\n", header, org_address, org);
#endif // __DEBUG__
}
