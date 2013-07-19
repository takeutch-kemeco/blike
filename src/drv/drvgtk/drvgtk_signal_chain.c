#include <glib.h>
#include "config.h"

#include "drvgtk_pthread.h"
#include "drvgtk_signal_chain.h"

static void resize_window(struct DrvGtkPthreadData *a, gint width, gint height)
{
        resize_MainWindow(a->main_window, width, height);
        show_MainWindow(a->main_window);
}

static void show_window(struct DrvGtkPthreadData *a)
{
        show_MainWindow(a->main_window);
}

static inline void disable_sse_flash_window(guchar *dst, guint32 *src, gint i)
{
        while (i-->0) {
                *dst++ = ((*src)>>16) & 0xFF;
                *dst++ = ((*src)>>8 ) & 0xFF;
                *dst++ = ((*src)>>0 ) & 0xFF;

                src++;
        }
}

static inline void enable_sse_flash_window(guchar *dst, guint32 *src, gint _i)
{
#ifdef HAVE_SSE2
        gint ii = (_i % 4) + 2;
        gint i = (_i - ii) / 4;

        const guint32 __attribute__((aligned(16)))mskR[4] = {0xFF,     0xFF,     0xFF,     0xFF};
        const guint32 __attribute__((aligned(16)))mskG[4] = {0xFF00,   0xFF00,   0xFF00,   0xFF00};
        const guint32 __attribute__((aligned(16)))mskB[4] = {0xFF0000, 0xFF0000, 0xFF0000, 0xFF0000};

        __asm__ volatile(
                "movdqa (%0), %%xmm5;"
                "movdqa (%1), %%xmm6;"
                "movdqa (%2), %%xmm7;"
                :
                :"r"(mskR), "r"(mskG), "r"(mskB)
        );

        while(i-->0){
                __asm__ volatile(
                        "prefetchnta 1024(%0);"

                        "movdqa (%0),   %%xmm0;"
                        "movdqa %%xmm0, %%xmm1;"
                        "movdqa %%xmm0, %%xmm2;"

                        "psrld $16, %%xmm0;"
                        "pslld $16, %%xmm2;"

                        "andpd %%xmm5, %%xmm0;"
                        "andpd %%xmm6, %%xmm1;"
                        "andpd %%xmm7, %%xmm2;"

                        "orpd   %%xmm2, %%xmm1;"
                        "orpd   %%xmm1, %%xmm0;"


                        "movq %%xmm0, 0(%1);"

                        "pshufd $0xE5, %%xmm0, %%xmm3;"
                        "movq %%xmm3, 3(%1);"

                        "pshufd $0xE6, %%xmm0, %%xmm4;"
                        "movq %%xmm4, 6(%1);"

                        "pshufd $0xE7, %%xmm0, %%xmm3;"
                        "movq %%xmm3, 9(%1);"
                        :
                        :"r"(src), "r"(dst)
                        :"%eax", "memory"
                );

                src += 4;
                dst += 12;
        }

        disable_sse_flash_window(dst, src, ii);
#endif /* HAVE_SSE2 */
}

static void flash_window_whole_area(struct DrvGtkPthreadData *a,
                                    gpointer src_frame_buffer)
{
        guint32 *src = (guint32*)src_frame_buffer;
        guchar *dst = (guchar*)a->main_window->frame_buffer;

        const guint area_len =
                a->main_window->frame_buffer_width * a->main_window->frame_buffer_height;

        if(src != NULL) {
#ifdef HAVE_SSE2
                enable_sse_flash_window(dst, src, area_len);
#else
                disable_sse_flash_window(dst, src, area_len);
#endif /* HAVE_SSE2 */
        }
}

static void flash_window_part_area(struct DrvGtkPthreadData *a,
                                   gpointer src_frame_buffer,
                                   const gint x,
                                   const gint y,
                                   const gint width,
                                   const gint height)
{
        const guint32 src_top_ofst = (y * a->main_window->frame_buffer_width) + x;
        const guint32 dst_top_ofst = src_top_ofst * 3;

        guint32 *src = ((guint32*)src_frame_buffer) + src_top_ofst;
        guchar *dst = ((guchar*)a->main_window->frame_buffer) + dst_top_ofst;

        const guint32 src_next_ofst = a->main_window->frame_buffer_width;
        const guint32 dst_next_ofst = src_next_ofst * 3;

        const guint line_len = width;

        if(src == NULL)
                return;

        int j;
        for (j = 0; j < height; j++) {
                /* x, y 位置によって開始位置が128bit単位アラインでなくなる場合は、
                 * SSE2 による flash を行うためには開始位置のアライン調整が必要だが
                 * そのような関数は面倒なので書いてないので、常に非SSE2版を用いる。
                 */
                disable_sse_flash_window(dst, src, line_len);

                src += src_next_ofst;
                dst += dst_next_ofst;
        }
}

static inline void round_range(gint *a, const gint min, const gint max)
{
        if (*a > max)
                *a = max;
        else if (*a < min)
                *a = min;
}

static void flash_window(struct DrvGtkPthreadData *a,
                         gpointer src_frame_buffer,
                         gint x,
                         gint y,
                         gint width,
                         gint height)
{
        round_range(&x, 0, a->main_window->frame_buffer_width - 1);
        round_range(&y, 0, a->main_window->frame_buffer_height - 1);

        round_range(&width, 0, a->main_window->frame_buffer_width - x);
        round_range(&height, 0, a->main_window->frame_buffer_height - y);

        if (x == 0 &&
            y == 0 &&
            width == a->main_window->frame_buffer_width &&
            height == a->main_window->frame_buffer_height)
                flash_window_whole_area(a, src_frame_buffer);
        else
                flash_window_part_area(a, src_frame_buffer, x, y, width, height);

        redraw_MainWindow(a->main_window);
}

static void exit_window(struct DrvGtkPthreadData *a)
{
        gtk_main_quit();
}

gboolean update_DrvGtkSignalChain(gpointer data)
{
        struct DrvGtkPthreadData *a = (struct DrvGtkPthreadData*)data;

        (*(a->time_count)) += DRVGTK_SYGNAL_CHECK_INTERVAL;

        if(a->signal->resize_window.ready == TRUE) {
                resize_window(a, a->signal->resize_window.width, a->signal->resize_window.height);
                a->signal->resize_window.ready = FALSE;
        }

        if(a->signal->show_window.ready == TRUE) {
                show_window(a);
                a->signal->show_window.ready = FALSE;
        }

        if(a->signal->flash_window.ready == TRUE) {
                flash_window(a,
                             a->signal->flash_window.src_frame_buffer,
                             a->signal->flash_window.x,
                             a->signal->flash_window.y,
                             a->signal->flash_window.width,
                             a->signal->flash_window.height);
                a->signal->flash_window.ready = FALSE;
        }

        if(a->signal->exit_window.ready == TRUE) {
                exit_window(a);
                a->signal->exit_window.ready == FALSE;
        }

        return TRUE;
}
