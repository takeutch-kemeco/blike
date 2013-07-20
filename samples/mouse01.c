#include <blike.h>

typedef void (*bld_callback_motion_notify_MainWindow)(void* a,
                                                      const double pos_x,
                                                      const double pos_y,
                                                      const double pressure,
                                                      const double angle_x,
                                                      const double angle_y);

typedef void (*bld_callback_button_press_MainWindow)(void* a, const int button_number);
typedef void (*bld_callback_button_release_MainWindow)(void* a, const int button_number);

extern void __bld_set_callback_arg(void* a);
extern void __bld_set_callback_motion_notify(bld_callback_motion_notify_MainWindow func);
extern void __bld_set_callback_button_press(bld_callback_button_press_MainWindow func);
extern void __bld_set_callback_button_release(bld_callback_button_release_MainWindow func);

struct MyCallbackArg {
        int press_flag;

        double old_pos_x;
        double old_pos_y;
        double pos_x;
        double pos_y;

        double pressure;

        double angle_x;
        double angle_y;
};

static void my_motion_notify(void* __a,
                             const double pos_x,
                             const double pos_y,
                             const double pressure,
                             const double angle_x,
                             const double angle_y)
{
        struct MyCallbackArg* a = __a;

        a->old_pos_x = a->pos_x;
        a->old_pos_y = a->pos_y;

        a->pos_x = pos_x;
        a->pos_y = pos_y;

        a->pressure = pressure;

        a->angle_x = angle_x;
        a->angle_y = angle_y;

        switch (a->press_flag) {
        case 1:
                bl_setPix((int)(a->pos_x), (int)(a->pos_y), 0x000000);
                a->press_flag = 2;
                break;

        case 2:
                bl_setCol(0x000000);
                bl_drawLine((int)(a->old_pos_x), (int)(a->old_pos_y),
                            (int)(a->pos_x), (int)(a->pos_y));

                a->press_flag = 2;
                break;
        }
}

static void my_button_press(void* __a, const int button_number)
{
        struct MyCallbackArg* a = __a;

        if (button_number & (1 << 0))
                a->press_flag = 1;
}

static void my_button_release(void* __a, const int button_number)
{
        struct MyCallbackArg* a = __a;

        if (button_number & (1 << 0))
                a->press_flag = 0;
}

blMain()
{
        struct MyCallbackArg my_callback_arg = {0, 0, 0, 0, 0, 0,};
        __bld_set_callback_arg(&my_callback_arg);
        __bld_set_callback_motion_notify(my_motion_notify);
        __bld_set_callback_button_press(my_button_press);
        __bld_set_callback_button_release(my_button_release);

        bl_openWin(400, 240);

        bl_setCol(0xFFFFFF);
        bl_fillRect(640, 480, 0, 0);

        while(1) {
                bl_wait(16);
        }
}
