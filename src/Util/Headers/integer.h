/*
  Definition of integer
  Copyright (c) 2016 and beyond Francis G. McCabe
*/

#ifndef INTEGER_H_
#define INTEGER_H_

#include "config.h"

typedef int64 integer;
typedef unsigned char byte;
typedef uint64 uinteger;

typedef enum {
  positive,
  negative
} sign;

#include "object.h"

#define LARGE_INT64 ((integer)(((uinteger)-1)>>1u))
#define LARGE_INT61 ((integer)(((uinteger)-1)>>3u))
#define LARGE_INT62 ((integer)(((uinteger)-1)>>2u))
#define LARGE_INT32 ((int32)(((uint32)-1)>>1u))

#define INT32_DIGITS (10)
#define INT64_DIGITS (19)

typedef struct ix_object_ *ixPo;

extern integer ixVal(ixPo i);
extern ixPo newInteger(integer ix);

extern classPo ixClass;

#ifdef VERIFY_OBJECT
#define O_IX(c) ((ixPo)(checkCast((c),ixClass)))
#else
#define O_IX(c) ((ixPo)(c))
#endif
#endif // INTEGER_H_
