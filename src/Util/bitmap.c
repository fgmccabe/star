/*
 * bitmap mgt utilities
 */

#include "config.h"
#include "bitmap.h"

#include <stdlib.h>
#include <assert.h>

#define ALIGN(ptr,size) (((((integer)ptr)+size-1)/size)*(size))

typedef struct _bitMap_ {
  unsigned long *map;
  int size;
} BitMapRecord;

static poolPo bitMapPool;

#define BITS_PER_BYTE 8
#define BITSHIFT 3
#define MASK(C) ((1<<C)-1)
#define BITMASK ((1<<BITSHIFT)-1)

void initBitMaps()
{
  bitMapPool = newPool(sizeof(BitMapRecord),128);
}

bitMapPo createBitMap(int size)
{
  bitMapPo map = (bitMapPo)allocPool(bitMapPool);

  map->size = ALIGN(size,BITS_PER_BYTE)/BITS_PER_BYTE;
  map->map = (unsigned long*)malloc(map->size);
  for(int ix=0;ix<map->size;ix++)
    map->map[ix] = 0;
  return map;
}

void releaseBitMap(bitMapPo map)
{
  free(map->map);
  freePool(bitMapPool,map);
}

logical isBitSet(bitMapPo map,int bit)
{
  assert(bit<map->size*BITS_PER_BYTE);
  int ix = bit>>BITSHIFT;
  return (map->map[ix]&(1<<(bit&BITMASK)))!=0;
}

void setBit(bitMapPo map,int bit, logical val)
{
  assert(bit<map->size*BITS_PER_BYTE);

  int ix = bit>>BITSHIFT;
  if(val)
    map->map[ix]|=(1<<bit&BITMASK);
  else
    map->map[ix]&=~(1<<bit&BITMASK);
}

int allocGap(bitMapPo map,int size)
{
  assert(size<BITS_PER_BYTE);
  unsigned int sizeMask = MASK(size);

  for(int ix=0;ix<map->size;ix++){
    // Put together a sub-map
    int mask = (ix<map->size-1?map->map[ix+1]:0)<<BITSHIFT|map->map[ix];
    for(int jx=0;jx<BITS_PER_BYTE;jx++){
      if(((mask>>jx)&sizeMask)==sizeMask){
	mask &= ~(sizeMask<<jx);
	map->map[ix] = mask;
	if(ix<map->size-1)
	  map->map[ix+1] = mask>>BITSHIFT;
	return ix*BITS_PER_BYTE+jx;
      }
    }
  }
  return -1;				/* Could not allocate */
}
