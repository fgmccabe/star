//
// Created by Francis McCabe on 3/7/18.
//

#include <arith.h>
#include "locks.h"
#include "errorCodes.h"
#include "globals.h"

ReturnStatus g__newLock(heapPo h) {
  lockPo lck = allocateLock(h);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) lck};
}

ReturnStatus g__acquireLock(heapPo h, termPo xc, termPo a1, termPo a2) {
  lockPo lck = C_LOCK(a1);
  double tmout = floatVal(a2);

  if (acquireLock(lck, tmout) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eINVAL};
}

ReturnStatus g__waitLock(heapPo h, termPo xc, termPo a1, termPo a2) {
  lockPo lck = C_LOCK(a1);
  double tmout = floatVal(a2);

  if (waitLock(lck, tmout) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eINVAL};
}

ReturnStatus g__releaseLock(heapPo h, termPo xc, termPo a1) {
  lockPo lck = C_LOCK(a1);

  if (releaseLock(lck) == Ok)
    return (ReturnStatus) {.ret=Ok, .result=unitEnum};
  else
    return (ReturnStatus) {.ret=Error, .result=eINVAL};
}
