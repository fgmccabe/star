//
// Created by Francis McCabe on 3/5/18.
//

#ifndef STAR_LOCKVARP_H
#define STAR_LOCKVARP_H

#include "heap.h"
#include "termP.h"
#include "lockvar.h"
#include <pthread.h>

typedef struct lock_term {
  clssPo clss;                  // == integerClass
  long count;				/* The lock recursion count */
  pthread_t owner;      /* The current owner of the lock */
  pthread_mutex_t mutex;    /* The mutex itself */
  pthread_cond_t cond;      /* Condition variable */
} LockRecord;

void initLocks();

void initLock(lockPo kck);

#define LockCellCount CellCount(sizeof(LockRecord))

extern logical traceLock;

#endif //STAR_LOCKVARP_H
