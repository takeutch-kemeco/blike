#include "blike.h"

blMain()
{
        struct POINT {
                int x, y;
        } *point;
        static struct POINT t[16] = {
                { 196, 100 }, { 187,  61 }, { 164,  29 }, { 129,   9 }, {  90,   5 },
                {  53,  17 }, {  23,  44 }, {   7,  81 }, {   7, 119 }, {  23, 156 },
                {  53, 183 }, {  90, 195 }, { 129, 191 }, { 164, 171 }, { 187, 139 },
                { 196, 100 }
        };
        static int c[8] = {
                0x000000, 0xff0000, 0x00ff00, 0xffff00,
                0x0000ff, 0xff00ff, 0x00ffff, 0xffffff
        };
        int i, j;
        int x0, y0, col, dis;

        openWin(200, 200);
        setMode(BL_POR);
        for (i = 0; i <= 14; i++) {
                x0 = t[i].x;
                y0 = t[i].y;
                for (j = i + 1; j <= 15; j++) {
                        dis = j - i; /* 2つの点の距離 */
                        if (dis >= 8)
                                dis = 15 - dis; /* 逆回りに数える */
                        if (dis != 0) {
                                setCol(c[8 - dis]);
                                if (x0 <= t[j].x)
                                        drawLine(x0, y0, t[j].x, t[j].y);
                                else
                                        drawLine(t[j].x, t[j].y, x0, y0);
                        }
                }
        }
        wait(-1);
}
