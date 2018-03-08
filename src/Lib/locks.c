//
// Created by Francis McCabe on 3/7/18.
//

#include <arith.h>
#include "locks.h"

ReturnStatus g__newLock(processPo p, ptrPo tos) {
  lockPo lck = allocateLock(processHeap(p));

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) lck};
  return ret;
}

ReturnStatus g__acquireLock(processPo p, ptrPo tos) {
  lockPo lck = C_LOCK(tos[1]);
  double tmout = floatVal(tos[0]);

  return rtnStatus(p, acquireLock(lck, tmout), "lock problem");
}

ReturnStatus g__waitLock(processPo p, ptrPo tos) {
  lockPo lck = C_LOCK(tos[1]);
  double tmout = floatVal(tos[0]);

  return rtnStatus(p, waitLock(lck, tmout), "lock problem");
}

ReturnStatus g__releaseLock(processPo p, ptrPo tos) {
  lockPo lck = C_LOCK(tos[0]);

  return rtnStatus(p, releaseLock(lck), "lock problem");
}
