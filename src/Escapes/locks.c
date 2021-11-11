//
// Created by Francis McCabe on 3/7/18.
//

#include <arith.h>
#include "locks.h"

ReturnStatus g__newLock(heapPo h) {
  lockPo lck = allocateLock(h);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) lck};
}

ReturnStatus g__acquireLock(heapPo h, termPo a1, termPo a2) {
  lockPo lck = C_LOCK(a1);
  double tmout = floatVal(a2);

  return rtnStatus(h, acquireLock(lck, tmout), "lock problem");
}

ReturnStatus g__waitLock(heapPo h, termPo a1, termPo a2) {
  lockPo lck = C_LOCK(a1);
  double tmout = floatVal(a2);

  return rtnStatus(h, waitLock(lck, tmout), "lock problem");
}

ReturnStatus g__releaseLock(heapPo h, termPo a1) {
  lockPo lck = C_LOCK(a1);

  return rtnStatus(h, releaseLock(lck), "lock problem");
}
