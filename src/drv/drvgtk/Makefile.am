lib_LTLIBRARIES = libblike-drvgtk.la
INCLUDEDIR='../../../include'
libblike_drvgtk_la_CFLAGS = -I${INCLUDEDIR} `pkg-config gtk+-3.0 glib-2.0 --cflags`
libblike_drvgtk_la_SOURCES =		\
	drvgtk_main.c			\
	drvgtk_interface.c		\
	drvgtk_system.c			drvgtk_system.h			\
	drvgtk_signal_chain.c		drvgtk_signal_chain.h		\
	drvgtk_signal.c			drvgtk_signal.h			\
	drvgtk_pthread.c		drvgtk_pthread.h		\
	drvgtk_key_ring_buffer.c	drvgtk_key_ring_buffer.h	\
	drvgtk_transrate_keycode.c	drvgtk_transrate_keycode.h	\
	drvgtk_keyboard_state.c		drvgtk_keyboard_state.h		\
	drvgtk_sleep.c			drvgtk_sleep.h			\
	drvgtk_malloc.c			drvgtk_malloc.h			\
	main_window.c			main_window.h			\
	main_screen.c			main_screen.h			\
	hankaku_font.c			\
	drvgtk_language.h		\
	${INCLUDEDIR}/setjmp.h		\
	${INCLUDEDIR}/blike.h		\
	${INCLUDEDIR}/blike0.h		\
	${INCLUDEDIR}/blikedrv.h	\
	${INCLUDEDIR}/config.h

