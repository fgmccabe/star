#ifndef _UTILS_H_
#define _UTILS_H_

#include "integer.h"

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

#ifndef STMT_WRAP
#define STMT_WRAP(S) do S while(False)
#endif

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

static integer inline clamp(integer min, integer ix, integer max) {
  if (ix < min)
    return min;
  else if (ix > max)
    return max;
  else
    return ix;
}

extern char *genSym(char *prefix);

extern integer nextPrime(integer min);

extern void syserr(const char *msg);

extern retCode homeDir(char *user,char *buffer,integer bufLen);

#endif
