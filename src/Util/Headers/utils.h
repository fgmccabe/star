#ifndef _UTILS_H_
#define _UTILS_H_

extern void syserr(const char *msg);

#ifndef Null
#define Null ((void*)0)
#endif

#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((integer)ptr)+size-1)/size)*(size)==(integer)ptr)
#endif

#ifndef NumberOf
#define NumberOf(a) (sizeof(a)/sizeof(a[0]))
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

#endif
