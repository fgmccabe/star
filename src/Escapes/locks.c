//
// Created by Francis McCabe on 3/7/18.
//

#include <arith.h>
#include "locks.h"

ReturnStatus g__newLock(processPo p, heapPo h, ptrPo tos) {
  lockPo lck = allocateLock(h);

  return (ReturnStatus) {.ret=Ok, .result=(termPo) lck};
}

ReturnStatus g__acquireLock(processPo p, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  lockPo lck = C_LOCK(Arg1);
  double tmout = floatVal(Arg1);

  return rtnStatus(p, h, acquireLock(lck, tmout), "lock problem");
}

ReturnStatus g__waitLock(processPo p, heapPo h, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  lockPo lck = C_LOCK(Arg1);
  double tmout = floatVal(Arg2);

  return rtnStatus(p, h, waitLock(lck, tmout), "lock problem");
}

ReturnStatus g__releaseLock(processPo p, heapPo h, ptrPo tos) {
  lockPo lck = C_LOCK(tos[0]);

  return rtnStatus(p, h, releaseLock(lck), "lock problem");
}
