#ifndef _ARITH_H_
#define _ARITH_H_

#include "config.h"

// overflow safe arithmetic for C

#define __HALF_MAX_SIGNED(type) ((type)1 << (sizeof(type)*8-2))
#define __MAX_SIGNED(type) (__HALF_MAX_SIGNED(type) - 1 + __HALF_MAX_SIGNED(type))
#define __MIN_SIGNED(type) (-1 - __MAX_SIGNED(type))

#define __MIN(type) ((type)-1 < 1?__MIN_SIGNED(type):(type)0)
#define __MAX(type) ((type)~__MIN(type))

#define __MIN_WORD64 __MIN(word64)
#define __MAX_WORD64 __MAX(word64)

#define assign(dest,src) ({ \
  typeof(src) __x=(src); \
  typeof(dest) __y=__x; \
  (__x==__y && ((__x<1) == (__y<1))?(void)((dest)=__y),0:1); \
})

#define add_of(c,a,b) ({ \
  typeof(a) __a=a; \
  typeof(b) __b=b; \
  (__b)<1 ? \
    ((__MIN(typeof(c))-(__b)<=(__a))?assign(c,__a+__b):1) : \
    ((__MAX(typeof(c))-(__b)>=(__a))?assign(c,__a+__b):1); \
})

#define sub_of(c,a,b) ({ \
  typeof(a) __a=a; \
  typeof(b) __b=b; \
  (__b)<1 ? \
    ((__MAX(typeof(c))+(__b)>=(__a))?assign(c,__a-__b):1) : \
    ((__MIN(typeof(c))+(__b)<=(__a))?assign(c,__a-__b):1); \
})

static inline int umult32(uint32 a,uint32 b,uint32* c) {
  unsigned long long x=(unsigned long long)a*b;
  if (x>0xffffffff) return 0;
  *c=x&0xffffffff;
  return 1;
}

// return 0 if ok, 1 on overflow
static inline int mult64(int64 a,int64 b,int64* c){
  int32 a1 = (int32)((a)>>32);
  int32 a0 = (int32)(a);
  int32 b1 = (int32)((b)>>32);
  int32 b0 = (int32)b;
  if(a1 && b1)
    return 1;
  else{
    integer r = (integer)a1*b0+(integer)a0+b1;
    if(r>0xffffffff)
      return 1;
    else
      return add_of(c,r<<32,(integer)a0*b0)
  }
}

#endif
