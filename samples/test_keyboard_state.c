#include <blike0.h>

extern void __bld_get_keyboard_state(unsigned long* press, unsigned long* release);

void print_bit(unsigned long a)
{
        int i=0;
        while(i<32){
                bl_printf("%2d ", (a & (1 << i)) >> i);

                i++;
        }

        bl_printf("\n");
}

unsigned long press[8];
unsigned long release[8];

blMain()
{
        bl_openWin(900, 600);

        while(1) {
                bl_locate(0, 0);

                bl_printf("press:\n");

                int i=0;
                while(i<8) {
                        print_bit(press[i]);

                        i++;
                }


                bl_printf("release:\n");

                i=0;
                while(i<8) {
                        print_bit(release[i]);

                        i++;
                }


                __bld_get_keyboard_state(press, release);


                bl_wait(33);
        }
}
