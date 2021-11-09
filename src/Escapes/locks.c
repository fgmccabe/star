//
// Created by Francis McCabe on 3/7/18.
//

#include <arith.h>
#include "locks.h"

ReturnStatus g__newLock(processPo p, heapPo h) {
  lockPo lck = allocateLock(h);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) lck};
}

ReturnStatus g__acquireLock(processPo p, heapPo h, termPo a1, termPo a2) {
  lockPo lck = C_LOCK(a1);
  double tmout = floatVal(a2);

  return rtnStatus(p, h, acquireLock(lck, tmout), "lock problem");
}

ReturnStatus g__waitLock(processPo p, heapPo h, termPo a1, termPo a2) {
  lockPo lck = C_LOCK(a1);
  double tmout = floatVal(a2);

  return rtnStatus(p, h, waitLock(lck, tmout), "lock problem");
}

ReturnStatus g__releaseLock(processPo p, heapPo h, termPo a1) {
  lockPo lck = C_LOCK(a1);

  return rtnStatus(p, h, releaseLock(lck), "lock problem");
}
