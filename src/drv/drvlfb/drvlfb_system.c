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
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <fcntl.h>
#include <sys/user.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <linux/fb.h>
#include "drvlfb_system.h"

struct DRVLFB_SYSTEM* drvlfb_new_system(const char* fb_dev_name,
                                        const int window_width,
                                        const int window_height)
{
        struct DRVLFB_SYSTEM* a = malloc(sizeof(*a));

        if ((a->fh = open(fb_dev_name, O_RDWR)) == -1)
                bl_exit();

        struct stat st;
        if (fstat(a->fh, &st) == -1)
                bl_exit();

        if (!S_ISCHR(st.st_mode) || major(st.st_rdev) != 29 /* FB_MAJOR */)
                bl_exit();

        struct fb_var_screeninfo fb_var;
        if (ioctl(a->fh, FBIOGET_VSCREENINFO, &fb_var))
                bl_exit();

        struct fb_fix_screeninfo fb_fix;
        if (ioctl(a->fh, FBIOGET_FSCREENINFO, &fb_fix))
                bl_exit();

        a->screen_width  = fb_var.xres;
        a->screen_height = fb_var.yres;
        a->bytePerLine = fb_fix.line_length;

        a->soff = (uint32_t)(fb_fix.smem_start) & (~PAGE_MASK);
        a->slen = (fb_fix.smem_len + a->soff + ~PAGE_MASK) & PAGE_MASK;
        a->smem = mmap(NULL, a->slen, PROT_READ | PROT_WRITE, MAP_SHARED,
                       a->fh, (off_t)0);

        if (a->smem == (void*)-1)
                bl_exit();

        a->smem = (void*)a->smem + a->soff;

        a->window_width  = window_width;
        a->window_height = window_height;

        return a;
}

void drvlfb_close_system(struct DRVLFB_SYSTEM* a)
{
        if (a->smem != (void*)-1)
                munmap((caddr_t)((ptrdiff_t)a->smem & PAGE_MASK), a->slen);

        close(a->fh);
        free((void*)a);
}
