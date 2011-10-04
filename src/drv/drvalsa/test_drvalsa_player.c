#include <stdio.h>
#include <glib.h>
#include "drvalsa_player.h"



void* bld_malloc(int size) {return g_malloc(size);}
void  bld_free(void* a) {g_free(a);}

int main(int argc, char** argv)
{
	g_thread_init(NULL);
	
	
	drvalsa_init_player();
	
	short* pcm = (short*)g_malloc(1000000 * sizeof(*pcm));
	
	gint i=1000000;
	while(i-->0) {
		pcm[i] = rand();
	}
	
//	while(1){
		drvalsa_play_pcm(pcm, pcm, 1000);
//	}
	
	// ごみかたづけ
	drvalsa_close_player();

	return 0;
}
