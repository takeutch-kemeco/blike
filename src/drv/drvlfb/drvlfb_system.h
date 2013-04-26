#include <stdio.h>
#include <stdint.h>

#ifndef __DRVLFB_SYSTEM_H__
#define __DRVLFB_SYSTEM_H__

#define DRVLFB_SYGNAL_CHECK_INTERVAL (1000 / 250)

struct DRVLFB_SYSTEM {
        int fh;
        int screen_height;
        int screen_width;
        uint32_t bytePerLine;
        uint32_t soff;
        uint32_t slen;
        void *smem;

        int window_width;
        int window_height;
} __attribute__((aligned(16)));

extern struct DRVLFB_SYSTEM *drvlfb_system;

struct DRVLFB_SYSTEM* drvlfb_new_system(const char *fb_dev_name,
                                        const int  window_width,
                                        const int  window_height);
void drvlfb_close_system(struct DRVLFB_SYSTEM *a);

#endif // __DRVLFB_SYSTEM_H__
