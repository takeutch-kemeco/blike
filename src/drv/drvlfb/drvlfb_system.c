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
