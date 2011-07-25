#include <stdio.h>
#include <glib.h>
#include "drvalsa_player_thread.h"



void* bld_malloc(int size) {return g_malloc(size);}
void  bld_free(void* a) {g_free(a);}

int main(int argc, char** argv)
{
	g_thread_init(NULL);
	
	
	
	drvalsa_create_player_thread();
	
	
	
	short* pcm = (short*)g_malloc(1000000 * sizeof(*pcm));
	
	gint i=1000000;
	while(i-->0) {
		pcm[i] = rand();
	}

	i = 0;
	while(1){
		if(i<500){
			int inc = drvalsa_set_pcm_player_thread(pcm, pcm); 
			if(inc < 0) {
				g_printf("%d ", i);
			}
			else {
				g_printf("\n\n\n%d \n\n\n", i);
				pcm += drvalsa_period_size;
			}
		}
		
		if(i == 1000) {
			// ごみかたづけ
			drvalsa_close_player_thread();
		}
		else if(i > 1000) {
			break;
		}

		g_usleep((1000 * 1000) / 250);
		i++;
	}



	return 0;
}
