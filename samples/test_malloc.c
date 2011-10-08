#include <blike.h>
#include <bl_malloc.h>

blMain()
{
	int aligned = 1;
	void* p;
	
	while(1) {
 		p = bl_malloc_aligned(12345, aligned);

		unsigned int address = (unsigned int)p;
		bl_locate(10,8);
		bl_printf(
			"address = [%p], aligned = [%u], %p mod %u = [%u]\n",
			p,
			aligned,
			p,
			aligned,
			address % aligned
		);
		
		bl_free_aligned(p);
		aligned++;

		bl_wait(1000/10);
	}
}

