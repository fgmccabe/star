//
// Created by Francis McCabe on 3/5/18.
//

#ifndef CAFE_LOCKVAR_H
#define CAFE_LOCKVAR_H

#include "term.h"

typedef struct lock_term *lockPo;
extern clssPo lckClass;

extern lockPo C_LOCK(termPo t);

static inline logical isLock(termPo p) {
  return hasClass(p, lckClass);
}

extern lockPo allocateLock(heapPo H);
extern retCode acquireLock(lockPo l, double tmOut);
extern retCode releaseLock(lockPo l);
extern retCode waitLock(lockPo l, double tmOut);

#endif //CAFE_LOCKVAR_H
