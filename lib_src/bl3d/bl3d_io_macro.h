#define BL3D_SET_PIX(x, y, c) {bl_setPix((x), (y), (c));}

#define BL3D_GET_PIX(x, y, c, vram) {bl_slctWin((vram)); (c) = bl_getPix((x), (y)); bl_slctWin(0);}
