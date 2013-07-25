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

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <sys/mman.h>
#include <unistd.h>

static void* drvlfb_malloc_aligned(const size_t bytes, const size_t aligned)
{
        void *tmp;
        int err = posix_memalign(&tmp, (size_t)aligned, (size_t)bytes);
        if (err) {
                g_printf("err: drvlfb_malloc_aligned, posix_memalign()\n");
                exit(1);
        }

        return tmp;
}

void* drvlfb_malloc_aligned16(const size_t bytes)
{
        return drvlfb_malloc_aligned(bytes, 16);
}

void* drvlfb_malloc_0_aligned16(const size_t bytes)
{
        void* tmp = drvlfb_malloc_aligned16(bytes);

        char *p = (char*)tmp;
        int i;
        for (i = 0; i < bytes; i++)
                *p++ = 0;

        return tmp;
}

void drvlfb_free_aligned16(void* a)
{
        free(a);
}

void* drvlfb_malloc_rwe(const size_t bytes)
{
        size_t pagesize = sysconf(_SC_PAGE_SIZE);
        if (pagesize == -1) {
                g_printf("err: osecpu.c, drvlfb_malloc_rwe(), sysconf(_SC_PAGE_SIZE)\n");
                exit(1);
        }

        void* tmp = drvlfb_malloc_aligned(bytes, pagesize);

        int err = mprotect(tmp, bytes, PROT_READ | PROT_WRITE | PROT_EXEC);
        if (err) {
                g_printf("err: drvlfb_malloc_rwe(), mprotect\n");
                exit(1);
        }

        return tmp;
}
