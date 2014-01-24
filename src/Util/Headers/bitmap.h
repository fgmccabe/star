#ifndef _BITMAP_H_
#define _BITMAP_H_

#include <ooio.h>

typedef struct _bitMap_ *bitMapPo;

/**
 * Create a new bit map
 */
extern bitMapPo createBitMap(int size);

/**
 * Release a bitmap
 */
extern void releaseBitMap(bitMapPo map);

/**
 * check to see if a given bit is set
 */
extern logical isBitSet(bitMapPo map,int bit);

/**
 * set a particular bit
 */
extern void setBit(bitMapPo map,int bit, logical val);

/**
 * Allocate a block of bits
 */
extern int allocGap(bitMapPo map,int size);

#endif
