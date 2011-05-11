#include "blikedrv.h"

struct BL_WORK bl_work;
#define	w	bl_work

void bl_init()
{
	int i;
	w.slctwin = 0;
	for (i = 0; i <= BL_SYSWIN; i++) {
		w.win[i].xsiz = 0;
		w.win[i].ysiz = 0;
		w.win[i].buf = NULL;
	}
	w.cx = w.cy = 0;
	w.col0 = 0xafafaf;
	w.col1 = 0x000000;
	w.tabsiz = 8;
//	w.ftyp = NULL;
	w.fptn = NULL;
	w.kbuf = NULL;
	w.ftyp = bld_malloc(bld_maxfonts());
	w.fptn = bld_malloc(bld_maxfonts() * sizeof (unsigned char *));
	w.kbuf = bld_malloc(BL_SIZ_KBUF * sizeof (int));
	if (w.ftyp == NULL || w.fptn == NULL || w.kbuf == NULL) {
		BL_EXIT
	}
	for (i = 0; i < 65536 * 4; i++) {
		w.ftyp[i] = 0;
		w.fptn[i] = NULL;
	}
	w.tmcount0 = w.tmcount = 0;
	w.kbuf_rp = w.kbuf_wp = w.kbuf_c = 0;
	w.mod = BL_PSET | BL_FULLHEIGHT | BL_DEBUG;
	return;
}

void bl_exit()
{
	int i;
	for (i = 0; i <= BL_DBGWIN; i++) {
		if (w.win[i].buf != NULL) {
			bld_free(w.win[i].buf, w.win[i].xsiz * w.win[i].ysiz * sizeof (int));
			w.win[i].buf = NULL;
		}
	}
	if (w.cbuf != NULL) {
		bld_free(w.cbuf, w.csiz_x * w.csiz_y * sizeof (int));
		w.cbuf = NULL;
	}
	if (w.ccol != NULL) {
		bld_free(w.ccol, w.csiz_x * w.csiz_y * sizeof (int));
		w.ccol = NULL;
	}
	if (w.cbak != NULL) {
		bld_free(w.cbak, w.csiz_x * w.csiz_y * sizeof (int));
		w.cbak = NULL;
	}
	if (w.ftyp != NULL) {
		bld_free(w.ftyp, bld_maxfonts());
		w.ftyp = NULL;
	}
	if (w.fptn != NULL) {
		bld_free(w.fptn, bld_maxfonts() * sizeof (unsigned char *));
		w.fptn = NULL;
	}
	if (w.kbuf != NULL) {
		bld_free(w.kbuf, BL_SIZ_KBUF * sizeof (int));
		w.kbuf = NULL;
	}
	w.win[0].xsiz = 0;
	w.win[BL_DBGWIN].xsiz = 0;
	w.win[BL_SLCTWIN].xsiz = 0;
	w.win[BL_SYSWIN].xsiz = 0;
	return;
}

int bl_main()
{
	if (setjmp(w.jb) == 0) {
		bl_init();
		blMain();
	}
	bl_exit();
	return 0;
}
