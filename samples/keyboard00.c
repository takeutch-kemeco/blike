#include <blike0.h>

typedef void (*bld_callback_key_press_MainWindow)(void* a, const int keyval);
typedef void (*bld_callback_key_release_MainWindow)(void* a, const int keyval);

extern void __bld_set_callback_arg(void* a);
extern void __bld_set_callback_key_press(bld_callback_key_press_MainWindow func);
extern void __bld_set_callback_key_release(bld_callback_key_release_MainWindow func);

struct MyCallbackArg {
        int aaa;
};

static void my_key_press(void* __a, const int keyval)
{
        struct MyCallbackArg* a = __a;

        bl_locate(0, 0);
        bl_printf("my_key_press(), keyval:[%d]\n", keyval);
}

static void my_key_release(void* __a, const int keyval)
{
        struct MyCallbackArg* a = __a;

        bl_printf("my_key_release(), keyval:[%d]\n", keyval);
}

blMain()
{
        struct MyCallbackArg my_callback_arg;
        __bld_set_callback_arg(&my_callback_arg);
        __bld_set_callback_key_press(my_key_press);
        __bld_set_callback_key_release(my_key_release);

        bl_openWin(400, 240);

        while(1) {
                bl_wait(16);
        }
}
