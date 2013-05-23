#include <glib.h>
#include <gdk/gdkkeysyms.h>
#include "config.h"

#include "drvgtk_language.h"
#include "drvgtk_key_ring_buffer.h"
#include "drvgtk_keyboard_state.h"

static void set_true_bit_DrvGtkKeybordState(struct DrvGtkKeybordState *a,
                                            guint32 shift)
{
        shift &= 0xFFFF;

        guint32 index = shift / 32;
        shift %= 32;

        a->value[index] |= (1 << shift);
}

// a |= b
static void or_DrvGtkKeybordState(struct DrvGtkKeybordState *a,
                                  struct DrvGtkKeybordState *b)
{
        gint i = 8;
        while (i-->0)
                a->value[i] |= b->value[i];
}

// a &= b
static void and_DrvGtkKeybordState(struct DrvGtkKeybordState *a,
                                   struct DrvGtkKeybordState *b)
{
        gint i = 8;
        while (i-->0)
                a->value[i] &= b->value[i];
}

// a = !a
static void not_DrvGtkKeybordState(struct DrvGtkKeybordState *a)
{
        gint i = 8;
        while (i-->0)
                a->value[i] = ~(a->value[i]);
}

// a = 0
static void zero_DrvGtkKeybordState(struct DrvGtkKeybordState *a)
{
        gint i = 8;
        while (i-->0)
                a->value[i] = 0;
}



static struct DrvGtkKeybordState* new_key_transform_table_ja(void)
{
        struct DrvGtkKeybordState *a = g_malloc0(sizeof(*a) * 0xFFFF);

        /* 一段目 */
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Escape],             VK_ESCAPE);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F1],                 VK_F1);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F2],                 VK_F2);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F3],                 VK_F3);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F4],                 VK_F4);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F5],                 VK_F5);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F6],                 VK_F6);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F7],                 VK_F7);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F8],                 VK_F8);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F9],                 VK_F9);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F10],                VK_F10);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F11],                VK_F11);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F12],                VK_F12);

        /* 二段目 */
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Zenkaku_Hankaku],    VK_PROCESSKEY);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_exclam],             VK_1);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_1],                  VK_1);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_doubleacute],        VK_2);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_2],                  VK_2);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_musicalsharp],       VK_3);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_3],                  VK_3);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_dollar],             VK_4);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_4],                  VK_4);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_percent],            VK_5);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_5],                  VK_5);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_ampersand],          VK_6);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_6],                  VK_6);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_apostrophe],         VK_7);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_7],                  VK_7);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_parenleft],          VK_8);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_8],                  VK_8);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_parenright],         VK_9);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_9],                  VK_9);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_0],                  VK_0);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_equal],              VK_OEM_MINUS);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_minus],              VK_OEM_MINUS);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_asciitilde],         VK_OEM_7);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_dead_circumflex],    VK_OEM_7);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_bar],                VK_OEM_5);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_backslash],          VK_OEM_5);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_BackSpace],          VK_BACK);

        /* 三段目 */
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Tab],                VK_TAB);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Q],                  VK_Q);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_q],                  VK_Q);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_W],                  VK_W);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_w],                  VK_W);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_E],                  VK_E);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_e],                  VK_E);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_R],                  VK_R);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_r],                  VK_R);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_T],                  VK_T);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_t],                  VK_T);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Y],                  VK_Y);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_y],                  VK_Y);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_U],                  VK_U);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_u],                  VK_U);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_I],                  VK_I);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_i],                  VK_I);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_O],                  VK_O);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_o],                  VK_O);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_P],                  VK_P);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_p],                  VK_P);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_at],                 VK_OEM_3);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_grave],              VK_OEM_3);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_braceleft],          VK_OEM_4);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_bracketleft],        VK_OEM_4);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Return],             VK_RETURN);

        /* 四段目 */
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Caps_Lock],          VK_CAPITAL);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_A],                  VK_A);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_a],                  VK_A);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_S],                  VK_S);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_s],                  VK_S);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_D],                  VK_D);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_d],                  VK_D);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_F],                  VK_F);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_f],                  VK_F);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_G],                  VK_G);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_g],                  VK_G);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_H],                  VK_H);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_h],                  VK_H);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_J],                  VK_J);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_j],                  VK_J);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_K],                  VK_K);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_k],                  VK_K);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_L],                  VK_L);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_l],                  VK_L);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_plus],               VK_OEM_PLUS);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_semicolon],          VK_OEM_PLUS);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_asterisk],           VK_OEM_1);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_colon],              VK_OEM_1);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_braceright],         VK_OEM_6);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_bracketright],       VK_OEM_6);

        /* 五段目 */
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Shift_L],            VK_LSHIFT);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Z],                  VK_Z);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_z],                  VK_Z);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_X],                  VK_X);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_x],                  VK_X);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_C],                  VK_C);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_c],                  VK_C);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_V],                  VK_V);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_v],                  VK_V);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_B],                  VK_B);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_b],                  VK_B);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_N],                  VK_N);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_n],                  VK_N);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_M],                  VK_M);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_m],                  VK_M);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_less],               VK_OEM_COMMA);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_comma],              VK_OEM_COMMA);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_greater],            VK_OEM_PERIOD);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_period],             VK_OEM_PERIOD);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_question],           VK_OEM_2);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_slash],              VK_OEM_2);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_underscore],         VK_OEM_102);
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_backslash],          VK_OEM_102);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Shift_R],            VK_RSHIFT);

        /* 六段目 */
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Control_L],          VK_LCONTROL);

//      set_true_bit_DrvGtkKeybordState(&a[],                           VK_LWIN);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Alt_L],              VK_LMENU);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Muhenkan],           VK_NONCONVERT);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_space],              VK_SPACE);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Mae_Koho],           VK_PROCESSKEY);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Hiragana_Katakana],  VK_OEM_COPY);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Alt_R],              VK_RMENU);

//      set_true_bit_DrvGtkKeybordState(&a[],                           VK_RWIN);

//      set_true_bit_DrvGtkKeybordState(&a[],                           VK_APPS);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Control_R],          VK_RCONTROL);

        /* 右側 */
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Print],              VK_SNAPSHOT);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Scroll_Lock],        VK_SCROLL);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Pause],              VK_PAUSE);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Insert],             VK_INSERT);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Home],               VK_HOME);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Page_Up],            VK_PRIOR);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Delete],             VK_DELETE);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_End],                VK_END);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Next],               VK_NEXT);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Up],                 VK_UP);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Left],               VK_LEFT);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Down],               VK_DOWN);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Right],              VK_RIGHT);

        /* テンキー */
        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Num_Lock],           VK_NUMLOCK);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_Divide],          VK_DIVIDE);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_Multiply],        VK_MULTIPLY);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_Subtract],        VK_SUBTRACT);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_7],               VK_NUMPAD7);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_8],               VK_NUMPAD8);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_9],               VK_NUMPAD9);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_Add],             VK_ADD);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_4],               VK_NUMPAD4);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_5],               VK_NUMPAD5);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_6],               VK_NUMPAD6);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_1],               VK_NUMPAD1);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_2],               VK_NUMPAD2);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_3],               VK_NUMPAD3);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_Return],             VK_RETURN);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_0],               VK_NUMPAD0);

        set_true_bit_DrvGtkKeybordState(&a[GDK_KEY_KP_Decimal],         VK_DECIMAL);

        return a;
}

static struct DrvGtkKeybordState* new_key_transform_table_en(void)
{
        struct DrvGtkKeybordState *a = g_malloc0(sizeof(*a) * 0xFFFF);

        // prease inpriment

        return a;
}

static struct DrvGtkKeybordState* new_key_transform_table_null(void)
{
        struct DrvGtkKeybordState *a = g_malloc0(sizeof(*a) * 0xFFFF);

        return a;
}



struct DrvGtkKeybordState* new_transform_table_DrvGtkKeybordState(guint32 language)
{
        struct DrvGtkKeybordState *a;

        switch(language) {
        case DRVGTK_KEYBORD_STATE_LANGUAGE_EN:
                a = new_key_transform_table_en();
                break;

        case DRVGTK_KEYBORD_STATE_LANGUAGE_JA:
                a = new_key_transform_table_ja();
                break;

        default:
                a = new_key_transform_table_null();
                break;
        }

        return a;
}



void add_DrvGtkKeybordState(struct DrvGtkKeybordState *press,
                            struct DrvGtkKeybordState *release,
                            struct DrvGtkKeybordState *table,
                            struct DrvGtkKey *key)
{
        switch(key->state) {
        case DrvGtkKeyState_none:
                break;

        case DrvGtkKeyState_press:
                if(key->value <= 0xFFFF)
                        or_DrvGtkKeybordState(press, &(table[key->value]));

                break;

        case DrvGtkKeyState_release:
                if(key->value <= 0xFFFF)
                        or_DrvGtkKeybordState(release, &(table[key->value]));

                break;
        }
}

/*
 *              p   10001010111
 *              r   00011101011
 *
 *              p   10001010111
 *      and     !r  11100010100
 *      _______________________
 *              p   10000010100
 *
 *              r   00000000000
 */
void next_DrvGtkKeybordState(struct DrvGtkKeybordState *press,
                             struct DrvGtkKeybordState *release)
{
        not_DrvGtkKeybordState(release);
        and_DrvGtkKeybordState(press, release);

        zero_DrvGtkKeybordState(release);
}
