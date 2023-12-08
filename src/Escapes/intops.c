//
// Created by Francis McCabe on 1/6/18.
// Integer operation escapes
//


#include <strings.h>
#include <stdlib.h>
#include <globals.h>
#include "ooio.h"
#include "engine.h"
#include "arithP.h"
#include "errorCodes.h"
#include "arithmetic.h"

ReturnStatus g__int_plus(heapPo h, termPo a1, termPo a2) {
  termPo Rs = makeInteger(integerVal(a1) + integerVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_minus(heapPo h, termPo a1, termPo a2) {
  termPo Rs = makeInteger(integerVal(a1) - integerVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_times(heapPo h, termPo a1, termPo a2) {
  termPo Rs = makeInteger(integerVal(a1) * integerVal(a2));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_div(heapPo h, termPo xc, termPo a1, termPo a2) {
  integer denom = integerVal(a1);
  integer numerator = integerVal(a2);

  if (numerator == 0) {
    return (ReturnStatus) {.ret=Error, .result=divZero};
  } else {
    termPo Rs = makeInteger(denom / numerator);
    return (ReturnStatus) {.ret=Ok, .result=Rs};
  }
}

ReturnStatus g__int_mod(heapPo h, termPo xc, termPo a1, termPo a2) {
  integer denom = integerVal(a1);
  integer numerator = integerVal(a2);

  if (numerator == 0) {
    return (ReturnStatus) {.ret=Error, .result=divZero};
  } else {
    integer reslt = denom % numerator;

    termPo Rs = makeInteger(reslt);

    return (ReturnStatus) {.ret=Ok, .result=Rs};
  }
}

ReturnStatus g__int_gcd(heapPo h, termPo xc, termPo a1, termPo a2) {
  integer gC = intGCD(integerVal(a1), integerVal(a2));

  if (gC > 0) {
    termPo g = makeInteger(gC);

    return (ReturnStatus) {.ret=Ok, .result=g};
  } else {
    return (ReturnStatus) {.ret=Error, .result=divZero};
  }
}

ReturnStatus g__band(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = makeInteger((integer) (Lhs & Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__basr(heapPo h, termPo a1, termPo a2) {
  integer Lhs = integerVal(a1);
  integer Rhs = integerVal(a2);

  termPo Rs = makeInteger(Lhs >> Rhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__blsl(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = makeInteger((integer) (Lhs << Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__blsr(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = makeInteger((integer) (Lhs >> Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bor(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = makeInteger((integer) (Lhs | Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bxor(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = makeInteger((integer) (Lhs ^ Rhs));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__bnot(heapPo h, termPo arg1) {
  termPo Rs = makeInteger(~(unsigned) integerVal(arg1));

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__nthb(heapPo h, termPo a1, termPo a2) {
  uint64 Lhs = (uint64) integerVal(a1);
  uint64 Rhs = (uint64) integerVal(a2);

  termPo Rs = (Lhs & ((unsigned) 1 << Rhs) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_eq(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (integerVal(a1) == integerVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_ge(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (integerVal(a1) >= integerVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_lt(heapPo h, termPo a1, termPo a2) {
  termPo Rs = (integerVal(a1) < integerVal(a2) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_abs(heapPo h, termPo a1) {
  integer Arg = integerVal(a1);
  termPo Rs = (Arg < 0 ? makeInteger(-Arg) : a1);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_hash(heapPo h, termPo Lhs) {
  integer Arg = integerVal(Lhs);
  termPo Rs = (Arg < 0 ? makeInteger(hash61(Arg)) : Lhs);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__int_lg2(heapPo h, termPo xc, termPo Lhs) {
  integer Arg = integerVal(Lhs);
  if (Arg <= 0) {
    return (ReturnStatus) {.ret=Error, .result=eRANGE};
  } else {
    return (ReturnStatus) {.ret=Ok, .result=makeInteger(lg2(Arg))};
  }
}

ReturnStatus g__bcount(heapPo h, termPo arg1) {
  integer Arg = integerVal(arg1);

  return (ReturnStatus) {.ret=Ok,
    .result=makeInteger(countBits(Arg))};
}

ReturnStatus g__int2str(heapPo h, termPo arg1) {
  integer ix = integerVal(arg1);
  char buff[64];

  integer len = int2StrByBase(buff, ix, 0, 10);
  termPo str = allocateString(h, buff, len);

  return (ReturnStatus) {.result = str, .ret=Ok};
}

ReturnStatus g__int_format(heapPo h, termPo xc, termPo a1, termPo a2) {
  integer ix = integerVal(a1);
  integer length;
  const char *fmt = strVal(a2, &length);
  char buff[64];
  integer pos = 0;

  retCode ret = formattedLong(ix, buff, &pos, NumberOf(buff), fmt, length);

  if (ret == Ok) {
    return (ReturnStatus) {.result = (termPo) allocateString(h, buff, pos), .ret=Ok};
  } else
    return (ReturnStatus) {.ret=Error, .result=eINVAL};
}

ReturnStatus g__int2flt(heapPo h, termPo arg1) {
  integer Arg = integerVal(arg1);
  termPo Rs = makeFloat((double) Arg);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__irand(heapPo h, termPo arg1) {
  integer mx = integerVal(arg1);
  integer rnd = randomInt();

  return (ReturnStatus) {.ret=Ok, .result=makeInteger(rnd % mx)};
}

ReturnStatus g__random(heapPo h) {
  double rnd = ((double) random()) / LARGE_INT32;

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) makeFloat(rnd)};
}

ReturnStatus g__seed(termPo arg1) {
  srandom((unsigned int) integerVal(arg1));
  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}

integer randomInt() {
  integer rnd = random();

  return rnd;
}
