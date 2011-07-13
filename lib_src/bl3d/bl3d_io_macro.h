#define bl3d_setPix(x, y, c) {bl_setPix((x), (y), (c));}

#define bl3d_getPix(x, y, c, vram) {bl_slctWin((vram)); (c) = bl_getPix((x), (y)); bl_slctWin(0);}
