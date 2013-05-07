#include "blikedrv.h"

#define w bl_work

unsigned int bl_clock()
{
        return w.tmcount;
}
