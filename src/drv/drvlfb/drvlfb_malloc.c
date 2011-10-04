#include <stdio.h>
#include <stdlib.h>

#include "drvlfb_build_flag.h"

typedef unsigned char uint8;
typedef unsigned long uint32;

static void* __drvlfb_malloc_aligned16(uint8* org)
{
	uint32 org_address = (uint32)org;
	uint32 diff_address = org_address % 16;


	uint32* header = (uint32*)(org + diff_address);
	*header = org_address;
	
	uint32* a = header + 4;        // header + 16byte


	uint32 a_address = (uint32)a;
	
	if((a_address % 16) !=0) {
		printf("org[%p], org_address[0x%lx], diff_address[%lu]\n", org, org_address, diff_address);

		printf(
			"header[%p], *header[0x%lx], ret_address[%p], *ret_address[%ld], ret_address mod 16[%ld]\n\n", 
			header, *header, a, *a, (*a) % 16
		);
		
		printf("err: drvlfb_malloc_aligned16()\n");
	}

	return (void*)a;
}

void* drvlfb_malloc_aligned16(size_t size)
{
	uint8* org = malloc(size + 64);

	return __drvlfb_malloc_aligned16(org);
}

void* drvlfb_malloc_0_aligned16(size_t size)
{
	uint8* org = calloc(1, size + 64);

	return __drvlfb_malloc_aligned16(org);
}

void drvlfb_free_aligned16(void* a)
{       
	uint32* header = ((uint32*)a) - 4;
	uint32 org_address = *header;  
	void* org = (uint32*)org_address;
}
