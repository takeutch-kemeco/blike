/* c_blike_01f_linux -
 * Copyright (C) 2011, 2012, 2013 Kemeco Takeutch <takeutchkemeco@gmail.com>
 * All rights reserved.
 *
 * c_blike_01f -
 * Copyright (C) 2011 H.Kawai (under KL-01)
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. Neither the name of the Kemeco Takeutch nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

#include "blikedrv.h"

void bl_setPtrn0(int sx, int sy, int sl, int ic, int *b, const unsigned char *c, const unsigned char *p, void (*errfnc)(const char *msg, unsigned char *nam))
{
        unsigned char elm0[256][8], nam[32];
        int elm1[256];
        int i, j, k, ssx = -1, ssy = -1, x, y;
        char f = 0;

        if (sl < 0) {
                sl = - sl;
                f = 1;
        }
        nam[0] = '\0';
        j = 0;
        for (;;) {
                while ('\0' < *c && *c <= ' ') c++;
                if (*c == '\0') break;
                if (*c == '%') {
                        if (c[1] == 'n' && c[2] == 'a' && c[3] == 'm' && c[4] == 'e' && (c[5] == '=' || c[5] <= ' ')) {
                                c += 5;
                                while ('\0' < *c && *c <= ' ') c++;
                                if (*c != '=') {
        err0:
                                        (*errfnc)("color-table error", nam);
                                }
                                c++;
                                while ('\0' < *c && *c <= ' ') c++;
                                i = 0;
                                nam[31] = '\0';
                                while (c[i] > ' ') {
                                        if (i == 31) goto err0;
                                        nam[i] = c[i];
                                        i++;
                                }
                                nam[i] = '\0';
                                c += i;
                                continue;
                        }
                        if (c[1] == 's' && c[2] == 's' && c[3] == 'x' && (c[4] == '=' || c[4] <= ' ')) {
                                c += 5;
                                while ('\0' < *c && *c <= ' ') c++;
                                if (*c != '=') goto err0;
                                c++;
                                while ('\0' < *c && *c <= ' ') c++;
                                i = 0;
                                while (*c > ' ') {
                                        if ('0' <= *c && *c <= '9') {
                                                i = i * 10 + (*c - '0');
                                        } else
                                                goto err0;
                                        c++;
                                }
                                ssx = i;
                                continue;
                        }
                        if (c[1] == 's' && c[2] == 's' && c[3] == 'y' && (c[4] == '=' || c[4] <= ' ')) {
                                c += 5;
                                while ('\0' < *c && *c <= ' ') c++;
                                if (*c != '=') goto err0;
                                c++;
                                while ('\0' < *c && *c <= ' ') c++;
                                i = 0;
                                while (*c > ' ') {
                                        if ('0' <= *c && *c <= '9') {
                                                i = i * 10 + (*c - '0');
                                        } else
                                                goto err0;
                                        c++;
                                }
                                ssy = i;
                                continue;
                        }
                }
                if (j >= 256) goto err0;
                i = 0;
                while (c[i] > ' ' && c[i] != '=') {
                        if (i == 8) goto err0;
                        elm0[j][i] = c[i];
                        i++;
                }
                elm0[j][i] = '\0';
                c += i;
                while ('\0' < *c && *c <= ' ') c++;
                if (*c != '=') goto err0;
                c++;
                while ('\0' < *c && *c <= ' ') c++;
                if (*c == '#') {
                        c++;
        hex:
                        i = 0;
                        while (*c > ' ') {
                                if ('0' <= *c && *c <= '9') {
                                        i = i * 16 + (*c - '0');
                                } else if ('A' <= *c && *c <= 'F') {
                                        i = i * 16 + (*c - ('A' - 10));
                                } else if ('a' <= *c && *c <= 'f') {
                                        i = i * 16 + (*c - ('a' - 10));
                                } else
                                        goto err0;
                                c++;
                        }
                } else if (*c == '0' && c[1] == 'x') {
                        c += 2;
                        goto hex;
                } else if (*c == '0' && c[1] == 'X') {
                        c += 2;
                        goto hex;
                } else if (*c == '-') {
                        c++;
                        i = 0;
                        while (*c > ' ') {
                                if ('0' <= *c && *c <= '9') {
                                        i = i * 10 + (*c - '0');
                                } else
                                        goto err0;
                                c++;
                        }
                        i = - i;
                } else if ('0' <= *c && *c <= '9') {
                        i = 0;
                        while (*c > ' ') {
                                if ('0' <= *c && *c <= '9') {
                                        i = i * 10 + (*c - '0');
                                } else
                                        goto err0;
                                c++;
                        }
                } else
                        goto err0;
                elm1[j] = i;
                j++;
        }

        k = x = y = 0;
        for (;;) {
                if (x >= sx) {
                        x -= sx;
                        y++;
                }
                if (y >= sy) break;
                for (i = 0; i < j; i++) {
                        for (k = 0; elm0[i][k] != '\0'; k++) {
                                if (elm0[i][k] != p[k]) break;
                        }
                        if (elm0[i][k] == '\0') break;
                }
                if (i >= j)
                        (*errfnc)("pattern-str error", nam);
                if (i != ic)
                        b[x + y * sl] = elm1[i];
                p += k;
                x++;
        }
        if (f != 0) {
                b[-2] = ssx;
                b[-1] = ssy;
        }
        return;
}
