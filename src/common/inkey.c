#include "blikedrv.h"

#define	w	bl_work

int bl_inkey1()
{
	return bl_inkey(BL_GETKEY | BL_CLEARREP | BL_DELFFF);
}

int bl_inkey(int flags)
{
	int i, j;
	if ((flags & BL_WAITKEYF) != 0) {
		BL_READY_WINDOW0
		bl_flshWin(w.win[0].xsiz, w.win[0].ysiz, 0, 0);
	}
	do {
		if ((flags & BL_WAITKEYNF) != 0) {
			while (w.kbuf_c <= 0)
				bl_waitNF(10);
		}
		if ((flags & BL_CLEARREP) != 0 && w.kbuf_c >= 2) {
			i = w.kbuf[w.kbuf_rp];
			if (0 < i && i < 0x0fff) {
				do {
					j = (w.kbuf_rp + 1) & ~(BL_SIZ_KBUF - 1);
					if (i != w.kbuf[j])
						break;
					bld_lock();
					w.kbuf_c--;
					w.kbuf_rp = (w.kbuf_rp + 1) & ~(BL_SIZ_KBUF - 1);
					bld_unlock();
				} while (w.kbuf_c >= 2);
			}
		}
		i = 0;
		if (w.kbuf_c > 0) {
			i = w.kbuf[w.kbuf_rp];
			if ((flags & BL_GETKEY) != 0) {
				bld_lock();
				w.kbuf_c--;
				w.kbuf_rp = (w.kbuf_rp + 1) & ~(BL_SIZ_KBUF - 1);
				bld_unlock();
			}
		}
	} while ((flags & BL_DELFFF) != 0 && i == 0x0fff);
	return i;
}

