#include <time.h>

void drvlfb_msleep(unsigned long msec)
{
        struct timespec req;
        req.tv_sec = 0;
        req.tv_nsec = msec * 1000 * 1000;
        nanosleep(&req, NULL);
}
