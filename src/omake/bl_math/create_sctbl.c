#include <stdio.h>
#include <math.h>
#include "bl_fast_sincos.c"

main()
{
	FILE* fp = fopen("sctbl.h", "wt");
	fprintf(fp,
		"#ifndef __SCTBL_H__\n"
		"#define __SCTBL_H__\n"
		"\n"
		"static float __attribute__((aligned(16)))"
		" sctbl[SCTBL_NUM][2] = {\n"
		);
	
	double u = (M_PI * 2) / SCTBL_NUM;
	int i;
	for(i = 0; i < SCTBL_NUM; i++) {
		fprintf(fp,
			"\t%10.10f, \t%10.10f,\n",
			sin(u * i),
			cos(u * i));
	}

	fprintf(fp,
		"};\n"
		"#endif // __SCTBL_H__\n"
		"\n"
		);

	fclose(fp);
	return 0;
}
