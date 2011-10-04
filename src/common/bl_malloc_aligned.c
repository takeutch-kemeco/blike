#include "blike.h"

typedef unsigned char uint8;
typedef unsigned long uint32;

extern void* bld_malloc(unsigned int bytes);
extern void bld_free(void* p, unsigned int bytes);

union Block {
	uint8 byte[32];

	struct {
		uint8* org;
		uint32 real_size;
		uint32 immediate_size;
	};
};

void* bl_malloc_aligned(unsigned int bytes, unsigned int align)
{
	const uint32 real_size = bytes + align + sizeof(union Block);
	uint8* org = bld_malloc(real_size);
	uint32 org_address = (uint32)org;

	const uint32 diff_size =
		align - ((org_address + sizeof(union Block)) % align);
	uint8* immediate_org = org + (sizeof(union Block) + diff_size);

	union Block* header =
		(union Block*)(immediate_org - sizeof(union Block));
	header->org = org;
	header->real_size = real_size;
	header->immediate_size = bytes;

	if((((uint32)immediate_org) % align) !=0) {
		bl_printf("err:bl_malloc_aligned()\n");
	}

	return (void*)immediate_org;
}

void bl_free_aligned(void* a)
{
	union Block* header = ((union Block*)a) - 1;

	bld_free((void*)header->org, header->real_size);
}
