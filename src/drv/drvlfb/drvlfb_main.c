#include "drvlfb_system.h"
#include "blikedrv.h"

struct BL_WORK __attribute__((aligned(16))) bl_work;

struct DRVLFB_SYSTEM* drvlfb_system;

int bl_argc;
char** bl_argv;

static char *DRVLFB_FB0_DEVICE_NAME = "/dev/fb0";

extern void bl_init();
extern void bl_exit();

int main(int argc, char** argv)
{
        bl_argc = argc;
        bl_argv = argv;

        drvlfb_system = drvlfb_new_system(DRVLFB_FB0_DEVICE_NAME, 640, 480);

        bl_init();

        blMain();

        bl_exit();
        return 0;
}
