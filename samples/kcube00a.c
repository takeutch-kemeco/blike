#include "blike.h"

#define PI 3.14159265358979324F
#define Deg2Rad (PI / 180.0F)

/* オブジェクトは、ただひとつの重心を持つ */
/* オブジェクトは、最大MAX_POINTS個の点を持つ */
/* オブジェクトは、最大MAX_POLYGONS枚のポリゴンを持つ */

/* ポリゴンは、ただひとつの重心を持つ */
/* ポリゴンは、最大MAX_VERTICES個の頂点を持つ */

#define MAX_POINTS   8
#define MAX_POLYGONS 6
#define MAX_VERTICES 4

#define WIDTH  160              /* 画面のＸ方向の大きさ */
#define HEIGHT 160              /* Ｙ方向の大きさ */

struct POINT3D {
        float x, y, z;
};

struct POINT2D {
        int x, y;
};

struct VERTEX {
        struct POINT3D p3d;
        struct POINT2D p2d; /* これは自動生成される */
};

struct POLYGON {
        int vertices; /* 頂点数 */
        int color;
        float center_z;
        struct VERTEX *vertex[MAX_VERTICES]; /* 右手系 */
};

#if 0

/* もしポリゴンによってvertices数にかなりばらつきがあり、
        上記のような方法がメモリの不経済を招くようなら、
        以下のような形式に改めるべきだろう */

struct POLYGON {
        int vertices; /* 頂点数 */
        int color;
        float center_z;
        struct VERTEX *vertex[3]; /* 右手系 */
};

polygon = malloc(sizeof (struct POLYGON) + 4 * (vertices - 3));

/* つまり(struct POLYGON)を可変長にしてしまう訳である */
/* この変更をした場合、当然(struct OBJECT)も変更しなければいけない */
/* 具体的には、

        struct POLYGON polygon[MAX_POLYGONS];

を

        struct POLYGON *polygon[MAX_POLYGONS];

に変更する。これに影響する部分はもちろんすべて変更する */

#endif

struct OBJECT {
        int polygons; /* ポリゴン数 */
        int vertices; /* 頂点数 */
        float center_z;
        struct VERTEX vertex[MAX_POINTS];
        struct POLYGON polygon[MAX_POLYGONS];
};

#if 0

/* もし物体によってポリゴン数や頂点数にかなりばらつきがあり、
        上記のような方法がメモリの不経済を招くようなら、
        以下のような形式に改めるべきだろう */

struct OBJECT {
        int polygons; /* ポリゴン数 */
        int vertices; /* 頂点数 */
        float center_z;
        struct VERTEX *vertex;
        struct POLYGON *polygon;
                /* 可変長にしているなら struct POLYGON **polygon; にする */
};

/* それで、vertexやpolygonの値は、必要量をmallocしてやることにする */

#endif

struct ZSORT {
        float z;
        void *p;
};

/* ポリゴンが移動したら、OBJECTの
        vertex[].p3d,
        center_z,
        polygon[].center_z
                を更新する必要がある */

struct SCREEN {
        float a, b; /* POINT3DをPOINT2Dに変換するための係数 */
        int x0, y0;
        int x_vsize, x_bsize, y_vsize; /* 描画範囲指定用 */
        int fillbuf[2][HEIGHT];
};

void draw_object(struct OBJECT *obj, struct SCREEN *scrn);
void draw_polygon(struct POLYGON *polygon, struct SCREEN *scrn);
void sort_z(int n, struct ZSORT *buf);

/* このmain()は物体が1つの場合しか考慮されていないが、
        複数の物体に対応する事は大して難しくないと思われる */

blMain()
{
        static struct POINT3D vertex[] = {
                {  100.0F,  100.0F,  100.0F }, {  100.0F,  100.0F, -100.0F },
                {  100.0F, -100.0F,  100.0F }, {  100.0F, -100.0F, -100.0F },
                { -100.0F,  100.0F,  100.0F }, { -100.0F,  100.0F, -100.0F },
                { -100.0F, -100.0F,  100.0F }, { -100.0F, -100.0F, -100.0F }
        };
        static unsigned char square[6][4] = {
                { 0, 4, 6, 2 }, { 1, 3, 7, 5 }, /* 右手系 */
                { 0, 2, 3, 1 }, { 0, 1, 5, 4 },
                { 4, 5, 7, 6 }, { 6, 7, 3, 2 }
        };
        struct OBJECT cube;
        struct SCREEN scrn;
        struct POLYGON *polygon;
        float theta_x, theta_y, theta_z;
        int     i, j;

        openWin(WIDTH, HEIGHT);

        cube.polygons = 6; /* ポリゴン数 */
        cube.vertices = 8; /* 頂点数 */
        for (i = 0; i < cube.polygons; i++) {
                static int col[] = { 0xff0000, 0x00ff00, 0xffff00, 0x0000ff, 0xff00ff, 0x00ffff };
                polygon = &cube.polygon[i];
                polygon->vertices = 4; /* 頂点数 */
                polygon->color = col[i];
                for (j = 0; j < polygon->vertices; j++)
                        polygon->vertex[j] = &cube.vertex[square[i][j]];
        }

        scrn.a = 150.0F;
        scrn.b = 400.0F;
        scrn.x0 = WIDTH / 2;
        scrn.y0 = HEIGHT / 2;
        scrn.x_vsize = scrn.x_bsize = WIDTH;
        scrn.y_vsize = HEIGHT;

        theta_x = theta_y = theta_z = 0.0F;

        for (;;) {
                float xa, xp, ya, yp, za, zp;
                float xt, yt, zt;
                theta_x += 1.0F * Deg2Rad;
                if (theta_x >= 2.0F * PI)
                        theta_x -= 2.0F * PI;
                theta_y += 1.5F * Deg2Rad;
                if (theta_y >= 2.0F * PI)
                        theta_y -= 2.0F * PI;
                theta_z += 2.0F * Deg2Rad;
                if (theta_z >= 2.0F * PI)
                        theta_z -= 2.0F * PI;
                xp = cos(theta_x); xa = sin(theta_x);
                yp = cos(theta_y); ya = sin(theta_y);
                zp = cos(theta_z); za = sin(theta_z);
                for (i = 0; i < cube.vertices; i++) {
                        struct POINT3D *v = &vertex[i], *p = &cube.vertex[i].p3d;
                        zt   = xp * v->z + xa * v->y; yt   = xp * v->y - xa * v->z;
                        xt   = yp * v->x + ya * zt;   p->z = yp * zt   - ya * v->x;
                        p->x = zp * xt   - za * yt;   p->y = zp * yt   + za * xt;
                }
                for (i = 0; i < cube.polygons; i++) {
                        float center_z = 0.0F;
                        polygon = &cube.polygon[i];
                        for (j = 0; j < polygon->vertices; j++)
                                center_z += polygon->vertex[j]->p3d.z;
                        polygon->center_z = center_z / polygon->vertices;
                }
                setCol(0x000000);
                fillRect(WIDTH, HEIGHT, 0, 0);
                draw_object(&cube, &scrn);
                wait(50);
        }
}

void draw_object(struct OBJECT *obj, struct SCREEN *scrn)
{
        int i, j;
        struct ZSORT sortbuf[MAX_POLYGONS];

        /* 3次元頂点座標を2次元投影 */
        for (i = 0; i < obj->vertices; i++) {
                float t = scrn->a / (scrn->b + obj->vertex[i].p3d.z);
                obj->vertex[i].p2d.x = scrn->x0 + (int) (obj->vertex[i].p3d.x * t);
                obj->vertex[i].p2d.y = scrn->y0 + (int) (obj->vertex[i].p3d.y * t);
        }

        /* ポリゴン描画順序をソート */
        for (i = 0; i < obj->polygons; i++) {
                sortbuf[i].z = obj->polygon[i].center_z;
                sortbuf[i].p = &obj->polygon[i];
        }
        sort_z(obj->polygons, sortbuf); /* zの値の大きいものが前に来る */

        /* ポリゴン描画ループ */
        for (i = 0; i < obj->polygons; i++) {
                struct POLYGON *polygon = sortbuf[i].p;
        //      struct POINT3D v[3], e[2];
                struct { float x, y; } v[3], e[2];

                /* 最初の3頂点の3次元座標を抽出 */
                for (j = 0; j < 3; j++) {
                        v[j].x = polygon->vertex[j]->p3d.x;
                        v[j].y = polygon->vertex[j]->p3d.y;
                //      v[j].z = polygon->vertex[j]->p3d.z;
                }

                /* 3頂点から、2つの辺ベクトルを算出 */
                e[0].x = v[1].x - v[0].x;
                e[0].y = v[1].y - v[0].y;
        //      e[0].z = v[1].z - v[0].z;
                e[1].x = v[2].x - v[1].x;
                e[1].y = v[2].y - v[1].y;
        //      e[1].z = v[2].z - v[1].z;

                /* 辺ベクトルの外積を使って、法線方向を求める */
                if (e[0].x * e[1].y - e[0].y * e[1].x <= 0.0F) {
                        /* ポリゴンの法線ベクトルのz成分は負 == こちら向き */
                        draw_polygon(polygon, scrn);
                }
        }
        return;
}

void sort_z(int n, struct ZSORT *buf)
/* オブジェクトのソートにも使える汎用型 */
{
        int i, j;
        for (i = 1; i < n; i++) {
                /* buf[i - 1].z〜buf[n].zまでの最大値を探す */
                int i1 = i - 1, max_i = i1;
                float max = buf[i1].z;
                for (j = i; j < n; j++) {
                        if (max < buf[j].z) {
                                max = buf[j].z;
                                max_i = j;
                        }
                }
                if (i1 != max_i) {
                        /* 交換 */
                        struct ZSORT tmp = buf[i1];
                        buf[i1] = buf[max_i];
                        buf[max_i] = tmp;
                }
        }
        return;
}

void draw_polygon(struct POLYGON *polygon, struct SCREEN *scrn)
{
        int i, y_min = 99999, y_max = -1, *fillbuf;
        struct POINT2D p0, p1;
        int x, dx;
        int y, dy;
        int *fillptr;
        int col;

        /* yが増加している区間は[0]へ。yが減少している区間は[1]へ。 */
        p0 = polygon->vertex[polygon->vertices - 1]->p2d;
        for (i = 0; i < polygon->vertices; i++, p0 = p1) {
                int y0, y1;
                p1 = polygon->vertex[i]->p2d;
                if (p1.y < y_min)
                        y_min = p1.y;
                if (p1.y > y_max)
                        y_max = p1.y;
                if (p0.y != p1.y) {
                        if (p0.y < p1.y) {
                                fillbuf = scrn->fillbuf[0];
                                y0 = p0.y;
                                y1 = p1.y;
                                dx = p1.x - p0.x;
                                x = p0.x;
                        } else {
                                fillbuf = scrn->fillbuf[1];
                                y0 = p1.y;
                                y1 = p0.y;
                                dx = p0.x - p1.x;
                                x = p1.x;
                        }
                        x <<= 16;
                        dx <<= 16;
                        dx /= y1 - y0;
                        if (dx >= 0)
                                x += 0x8000;
                        else
                                x -= 0x8000;
                        /* クリッピングしながらxの値を格納 */
                        if (0 <= y0 && y1 < scrn->y_vsize) {
                                /* クリッピング不用の高速ルーチン */
                                for (y = y0; y <= y1; y++, x += dx)
                                        fillbuf[y] = x >> 16;
                        } else {
                                /* この方法はあまり賢くない */
                                for (y = y0; y <= y1; y++, x += dx) {
                                        if (0 <= y && y < scrn->y_vsize) /* クリッピング */
                                                fillbuf[y] = x >> 16;
                                }
                        }
                }
        }

        /* fillbufを参照して塗りつぶし */
        setCol(polygon->color);
        if (y_min < 0)
                y_min = 0;
        if (y_max >= scrn->y_vsize)
                y_max = scrn->y_vsize - 1;
        for (y = y_min; y <= y_max; y++) {
                p0.x = scrn->fillbuf[0][y];
                p1.x = scrn->fillbuf[1][y];
#if 0
                if (p0.x > p1.x) {
                        i = p0.x;
                        p0.x = p1.x;
                        p1.x = i;
                }
                if (p0.x < 0)
                        p0.x = 0;
                if (p1.x >= scrn->x_vsize)
                        p1.x = scrn->x_vsize - 1;
                drawRect(p1.x - p0.x + 1, 1, p0.x, y);
#endif
                if (p0.x <= p1.x)
                        drawRect(p1.x - p0.x + 1, 1, p0.x, y);
                else
                        drawRect(p0.x - p1.x + 1, 1, p1.x, y);
        }
        return;
}
