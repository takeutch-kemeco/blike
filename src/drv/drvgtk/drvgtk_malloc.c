/* c_blike_01f_linux -
 * Copyright (C) 2011, 2012, 2013 Kemeco Takeutch <takeutchkemeco@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name of the Kemeco Takeutch nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

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
