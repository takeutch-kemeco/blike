#include <glib.h>

static gpointer __drvgtk_malloc_aligned16(guint8 *org)
{
        guint32 org_address = (guint32)org;
        guint32 diff_address = org_address % 16;


        guint32* header = (guint32*)(org + diff_address);
        *header = org_address;

        guint32* a = header + 4;        // header + 16byte


        guint32 a_address = (guint32)a;

        if ((a_address % 16) !=0) {
                g_printf("org[%p], org_address[0x%x], diff_address[[%ld]\n", org, org_address, diff_address);

                g_printf(
                        "header[%p], *header[0x%x], ret_address[%p], *ret_address[%ld], ret_address mod 16[%ld]\n\n",
                        header, *header, a, *a, (*a) % 16
                );

                g_printf("err: drvgtk_malloc_aligned16()\n");
        }

        return (gpointer)a;
}

gpointer drvgtk_malloc_aligned16(gsize size)
{
        guint8 *org = g_malloc(size + 64);

        return __drvgtk_malloc_aligned16(org);
}

gpointer drvgtk_malloc_0_aligned16(gsize size)
{
        guint8 *org = g_malloc0(size + 64);

        return __drvgtk_malloc_aligned16(org);
}

void drvgtk_free_aligned16(gpointer a)
{
        guint32 *header = ((guint32*)a) - 4;
        guint32 org_address = *header;
        gpointer org = (guint32*)org_address;
}
