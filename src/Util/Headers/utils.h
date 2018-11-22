#ifndef _UTILS_H_
#define _UTILS_H_

#include "integer.h"

extern void syserr(const char *msg);

#ifndef Null
#define Null ((void*)0)
#endif

#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((integer)(ptr))+(size)-1)/(size))*(size)==(integer)(ptr))
#endif

#ifndef NumberOf
#define NumberOf(a) (sizeof(a)/sizeof((a)[0]))
#endif

#ifndef AddressOf
#define AddressOf(tp, field) ((long)(void*)(&((tp*)0)->field))
#endif

static long inline minl(long a, long b) {
  if (a < b)
    return a;
  else
    return b;
}

static long inline maxl(long a, long b) {
  if (a > b)
    return a;
  else
    return b;
}

static integer inline minimum(integer a, integer b) {
  if (a < b)
    return a;
  else
    return b;
}

static integer inline maximum(integer a, integer b) {
  if (a > b)
    return a;
  else
    return b;
}

extern char *genSym(char * prefix);

extern integer nextPrime(integer min);

#endif
