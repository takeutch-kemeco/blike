#include "blike.h"

extern void __bld_get_keyboard_state(unsigned long* press, unsigned long* release);

void print_bit(unsigned long a)
{
        int i=0;
        while(i<32){
                printf("%2d ", (a & (1 << i)) >> i);

                i++;
        }

        printf("\n");
}

unsigned long press[8];
unsigned long release[8];

blMain()
{
        openWin(900, 600);

        while(1) {
                locate(0, 0);

                printf("press:\n");

                int i=0;
                while(i<8) {
                        print_bit(press[i]);

                        i++;
                }


                printf("release:\n");

                i=0;
                while(i<8) {
                        print_bit(release[i]);

                        i++;
                }


                __bld_get_keyboard_state(press, release);


                wait(33);
        }
}
