#include <blike0.h>
#include "HsFFI.h"

void hs_bl_main(void);
extern int bl_argc;
extern char** bl_argv;

int blMain()
{
        hs_init(&bl_argc, &bl_argv);

        hs_bl_main();

        hs_exit();

	return 0;
}

