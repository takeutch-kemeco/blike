struct BL_WIN {
	int xsiz, ysiz, *buf;
};

typedef int jmp_buf[3];

struct BL_WORK {
	struct BL_WIN win[19];
	jmp_buf jb;
	int csiz_x, csiz_y, cx, cy, col0, col1, tabsiz, slctwin;
	int tmcount, tmcount0, mod, rand_seed;
	int *cbuf;
	unsigned char *ftyp;
	unsigned char **fptn;
	int *ccol, *cbak;
	int *kbuf, kbuf_rp, kbuf_wp, kbuf_c;
};

extern struct BL_WORK __attribute__((aligned(16)))bl_work;

#define bl3d_setPix(x, y, c) {bl_work.win[0].buf[x + y * bl_work.win[0].xsiz] = c;}

#define bl3d_getPix(x, y) (bl_work.win[10].buf[x + y * bl_work.win[10].xsiz])
