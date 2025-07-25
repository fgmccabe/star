#ifndef UTILS_H_
#define UTILS_H_

#include "integer.h"

#ifndef Null
#define Null ((void*)0)
#endif

#ifndef ALIGNED
#define ALIGNED(ptr, size) (((((integer)(ptr))+(size)-1)/(size))*(size)==(integer)(ptr))
#endif

#ifndef ALIGNVALUE
#define ALIGNVALUE(count, size) ((((count)+(size)-1)/(size))*(size))
#endif

#ifndef NumberOf
#define NumberOf(a) (sizeof(a)/sizeof((a)[0]))
#endif

#ifndef OffsetOf
#define OffsetOf(tp, field) ((long)(void*)(&((tp*)0)->field))
#endif

#ifndef STMT_WRAP
#define STMT_WRAP(S) do S while(False)
#endif

static integer inline absolute(integer a) {
  if (a < 0)
    return -a;
  else
    return a;
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

static integer inline clamp(integer min, integer ix, integer max) {
  if (ix < min)
    return min;
  else if (ix > max)
    return max;
  else
    return ix;
}

static logical inline isEven(integer x) {
  return x % 2 == 0;
}

static logical inline isOdd(integer x) {
  return x % 2 == 1;
}

static logical inline is16bit(integer x) {
  return (((uinteger) x) & 0xffffu) == (uinteger) x;
}

typedef struct hwm_ {
  integer current;
  integer max;
} HwmRec, *hwmPo;

integer hwmOf(hwmPo hwm);

integer hwmBump(hwmPo hwm, integer delta);

extern char *genSym(const char *prefix, char *buffer, integer buffLen);

integer lg2(integer ix);
integer lg2Ceiling(integer ix);
integer countBits(integer ix);
integer intGCD(integer a, integer b);

integer nextPrime(integer min);

extern void syserr(const char *msg);

#ifndef NDEBUG
#define check(Tst, Msg) STMT_WRAP(if(!(Tst)){check_(__func__, __FILE__, __LINE__,#Tst,(Msg));})
#else
#define check(Tst, Msg)
#endif

extern void check_(const char *func, const char *srcFile, int line, char *frag, char *msg);

extern retCode homeDir(char *user, char *buffer, integer bufLen);

typedef enum {
  successCode = 0, /* Normal exit */
  failCode, /* Failing exit */
  errorCode,  // Error exit
  oomCode, // Out of memory
  undefinedCode, // tried to execute undefined code
  fiberCode, // Something wrong with fiber code
  singleCode, // something wrong with single assignment var
  assignmentCode, // something wrong with assignment
  specialMethodCode, // something wrong with a special method
  invalidOperationCode, // tried an invalid operation
  abortCode, // abort operation
} ExitCode;

void star_exit(ExitCode code);

#endif
