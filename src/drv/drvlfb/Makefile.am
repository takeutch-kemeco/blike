lib_LTLIBRARIES = libblike-drvlfb.la
OPTIMAIZE=-O4 -march=native -mtune=native
INCLUDEDIR='../../../include'
libblike_drvlfb_la_CFLAGS = -I${INCLUDEDIR}
libblike_drvlfb_la_SOURCES =		\
	drvlfb_main.c			\
	drvlfb_interface.c		\
	drvlfb_system.c			drvlfb_system.h			\
	drvlfb_sleep.c			drvlfb_sleep.h			\
	drvlfb_malloc.c			drvlfb_malloc.h			\
	hankaku_font.c			\
	${INCLUDEDIR}/setjmp.h		\
	${INCLUDEDIR}/blike.h		\
	${INCLUDEDIR}/blike0.h		\
	${INCLUDEDIR}/blikedrv.h	\
	${INCLUDEDIR}/config.h
