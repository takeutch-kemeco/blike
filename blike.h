#if (!defined(BLIKE_H))

#define BLIKE_H	1

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "blike0.h"

#if (defined(__cplusplus))
extern "C" {
#endif

#define putc		bl_putc
#define	puts		bl_puts1
#define	printf		bl_printf
#define	scanf		bl_scanf
#define malloc		bl_malloc
#define rand		bl_rand
#define srand		bl_srand
#define	gets		bl_gets
#define openWin		bl_openWin
#define setCol		bl_setCol
#define setBCol		bl_setBCol
#define rgb		bl_rgb
#define iCol		bl_iCol
#define flshWin		bl_flshWin
#define getGrpB		bl_getGrpB
#define setPix		bl_setPix
#define fillRect	bl_fillRect
#define drawRect	bl_drawRect
#define drawLine	bl_drawLine
#define rnd		bl_rnd
#define wait		bl_wait
#define color		bl_color
#define locate		bl_locate
#define getPix		bl_getPix
#define waitNF		bl_waitNF
#define inkey		bl_inkey1
#define cls		bl_cls
#define inptInt		bl_inptInt
#define inptFlot	bl_inptFlot
#define setMode		bl_setMode
#define fillOval	bl_fillOval
#define drawStr		bl_drawStr
#define openVWin	bl_openVWin
#define slctWin		bl_slctWin
#define copyRct0	bl_copyRct0
#define copyRct1	bl_copyRct1
#define	drawPtrn	bl_drawPtrn_d
#define	drawPtrnD	bl_drawPtrn_d
#define	drawPtrnR	bl_drawPtrn_r

#if (defined(__cplusplus))
}
#endif

#endif
