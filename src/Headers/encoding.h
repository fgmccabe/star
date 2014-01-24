/*
 * An encoding representation for Cafe byte code
 */

#ifndef _ENCODING_H_
#define _ENCODING_H_

#include "signature.h"

/*
 * A universal data value encoding scheme. This can be used to represent any
 * value.
 *
 * Many of the elements of this encoding depend on the representation of integer
 * values. 
 */

typedef enum {
  trmChr = 0x10,			/* What follows is a character */
  trmInt = 0x20,			/* What follows is a 32 bit int */
  trmFlt = 0x40,			/* What follows is a double float */
  trmStr = 0x50,			/* A length-encoded string */
  trmEsc = 0x70,			/* An escape reference */
  trmCde = 0x80,			/* A code block */
  trmDct = 0x90,			/* A dictionary */
  trmRef = 0xa0,			/* A reference to a label */
  trmTag = 0xb0				/* A label in a circular structure */
} elTag;

#define trmMask (0xf0)

retCode encodeInteger(ioPo out,int64 ix);
retCode encodeFloat(ioPo out,double dx);
retCode encodeString(ioPo out,uniChar *str);
retCode encodeRef(ioPo out,uniChar *str);
retCode encodeEscapeRef(ioPo out,uniChar *str);

static inline logical isTag(uniChar ch,elTag tag){
  return (ch&trmMask)==tag;
}

#endif
