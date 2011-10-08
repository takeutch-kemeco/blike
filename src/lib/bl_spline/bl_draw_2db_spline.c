#include <math.h>

extern void bl_printf(const char *s, ...);
extern void bl_drawLine(int x0, int y0, int x1, int y1);

struct VEC {
	float x, y;
};

static
void add_vec(struct VEC* dst, struct VEC* src)
{
	dst->x += src->x;
	dst->y += src->y;
}

static
void scale_vec(struct VEC* dst, float s)
{
	dst->x *= s;
	dst->y *= s;
}

static
float norm_vec(struct VEC* a)
{
	return sqrt(a->x * a->x + a->y * a->y);
}

static
void unit_vec(struct VEC* dst, struct VEC* src, float ir)
{
	dst->x = src->x * ir;
	dst->y = src->y * ir;
}

static
float round_float(float a)
{
	int tmp = (a > 0)? a + 0.5: a - 0.5;
	return (float)tmp;
}

static
void round_vec(struct VEC* dst, struct VEC* src)
{
	dst->x = round_float(src->x);
	dst->y = round_float(src->y);
}

/*
 * dst = A - B;
 */
static
void diff_vec(
	struct VEC* dst,
	struct VEC* A,
	struct VEC* B
)
{
	dst->x = A->x - B->x;
	dst->y = A->y - B->y;
}

void bl_draw_2db_spline(
	int x0, int y0,
	int x1, int y1,
	int x2, int y2
)
{
	struct VEC p0 = {x0, y0};
	struct VEC p1 = {x1, y1};
	struct VEC p2 = {x2, y2};

	struct VEC al;
	diff_vec(&al, &p1, &p0);
	struct VEC bl;
	diff_vec(&bl, &p2, &p1);

	float ar = norm_vec(&al);
	float br = norm_vec(&bl);

	float r = (ar > br) ? ar: br;
	if(r <= 0.0) {
		bl_printf("err : bl_draw2DBSpline()\n");
		return;
	}
	float ir = 1 / r;

	struct VEC au;
	unit_vec(&au, &al, ir);
	struct VEC bu;
	unit_vec(&bu, &bl, ir);

	struct VEC ap = p0;
	struct VEC bp = p1;
	
	struct VEC op = ap;
	
	int ie = (int)r;
	int i;
	for(i = 0; i < ie; i++) {
		struct VEC cl;
		diff_vec(&cl, &bp, &ap);
		struct VEC cu;
		unit_vec(&cu, &cl, ir);

		struct VEC p = ap;
		scale_vec(&cu, (float)i);
		add_vec(&p, &cu);

		round_vec(&p, &p);
		bl_drawLine(op.x, op.y, p.x, p.y);

		op = p;
		add_vec(&ap, &au);
		add_vec(&bp, &bu);
	}
}