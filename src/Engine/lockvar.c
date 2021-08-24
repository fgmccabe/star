//
// Created by Francis McCabe on 3/5/18.
//

#include "assert.h"

/*
  Lock synchronization functions
*/


#include <errno.h>
#include "code.h"
#include <termP.h>
#include <lockvarP.h>
#include <math.h>

#include "clock.h"

#define NANO (1000000000)

static long lockSize(specialClassPo cl, termPo o);
static termPo lockCopy(specialClassPo cl, termPo dst, termPo src);
static termPo lockScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical lockCmp(specialClassPo cl, termPo o1, termPo o2);
static integer lckHash(specialClassPo cl, termPo o);
static retCode lockDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo lockFinalizer(specialClassPo class, termPo o, void *cl);

logical traceLock = False;        /* true if tracing locks */

SpecialClass LockClass = {
  .clss = Null,
  .sizeFun = lockSize,
  .copyFun = lockCopy,
  .scanFun = lockScan,
  .finalizer = lockFinalizer,
  .compFun = lockCmp,
  .hashFun = lckHash,
  .dispFun = lockDisp
};

clssPo lckClass = (clssPo) &LockClass;

void initLocks() {
  LockClass.clss = specialClass;
}

lockPo allocateLock(heapPo H) {
  lockPo t = (lockPo) allocateObject(H, lckClass, LockCellCount);
  initLock(t);
  return t;
}

long lockSize(specialClassPo cl, termPo o) {
  return CellCount(sizeof(LockRecord));
}

termPo lockCopy(specialClassPo cl, termPo dst, termPo src) {
  lockPo si = C_LOCK(src);
  lockPo di = (lockPo) (dst);
  *di = *si;
  return (termPo) di + LockCellCount;
}

termPo lockScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return (termPo) (o + LockCellCount);
}

termPo lockFinalizer(specialClassPo class, termPo o, void *cl) {
  return (termPo) (o + LockCellCount);
}

logical lockCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
}

integer lckHash(specialClassPo cl, termPo o) {
  lockPo l = C_LOCK(o);
  return (integer) (&l->mutex);
}

static retCode lockDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  return outMsg(out, "<<lock@0x%x>>", t);
}

lockPo C_LOCK(termPo t) {
  assert(hasClass(t, lckClass));
  return (lockPo) t;
}


// We are having to do this somewhat clumsily because not all systems
// support pthread_mutex_timedlock (posix 1.d) including mac os x :(
// So we use a condition variable to signal that a lock is available

retCode acquireLock(lockPo l, double tmOut) {
#ifdef LOCKTRACE
  if (traceLock)
    outMsg(logFile, RED_ESC_ON "acquire lock (%f) [%d] on 0x%x"RED_ESC_OFF"\n%_", tmOut, l->count, l);
#endif

  if (pthread_mutex_lock(&l->mutex))  /* This will be transitory */
    syserr("problem in acquiring lock mutex");

  again:
  if (l->count == 0) {
    l->owner = pthread_self();
    l->count = 1;
    if (pthread_mutex_unlock(&l->mutex))
      syserr("problem in releasing lock mutex");
    return Ok;
  } else if (pthread_equal(l->owner, pthread_self())) {
    l->count++;

    if (pthread_mutex_unlock(&l->mutex))
      syserr("problem in releasing lock mutex");
    return Ok;
  } else if (tmOut == 0.0) {      /* treat as a no-timeout lock */
#ifdef LOCKTRACE
    if (traceLock)
      outMsg(logFile, RED_ESC_ON "getLock cond_wait on 0x%x"RED_ESC_OFF"\n%_", l);
#endif

    if (pthread_cond_wait(&l->cond, &l->mutex))
      syserr("problem in waiting");
    goto again;        /* try to lock again */
  } else {
    struct timespec tm;
    double seconds;
    double fraction = modf(tmOut, &seconds);

    tm.tv_sec = (long) seconds;
    tm.tv_nsec = (long) (fraction * NANO);  // Convert microseconds to nanoseconds
    switch (pthread_cond_timedwait(&l->cond, &l->mutex, &tm)) {
      case 0:
        goto again;
      case ETIMEDOUT:
        pthread_mutex_unlock(&l->mutex);
        return Fail;
      default:
        pthread_mutex_unlock(&l->mutex);
        return Error;
    }
  }
}

retCode releaseLock(lockPo l) {
  if (pthread_mutex_lock(&l->mutex))  /* This will be transitory */
    syserr("problem in acquiring lock mutex");
  retCode ret = Ok;

#ifdef LOCKTRACE
  if (traceLock)
    outMsg(logFile, RED_ESC_ON "releaseLock [%d] on 0x%x"RED_ESC_OFF"\n%_", l->count, l);
#endif

  if (pthread_equal(l->owner, pthread_self())) {
    if (l->count > 0) {
      l->count--;

      if (l->count <= 0) {
        l->count = 0;
        l->owner = (pthread_t) Null;
        pthread_cond_broadcast(&l->cond);
      }
    }
  } else if (l->count != 0) {
#ifdef LOCKTRACE
    if (traceLock)
      outMsg(logFile, RED_ESC_ON "tried to release non-owned lock 0x%x"RED_ESC_OFF"\n%_", l);
#endif

    ret = Fail;
  }

  if (pthread_mutex_unlock(&l->mutex))
    syserr("problem in releasing lock");
  return ret;
}

// waitLock releases a lock and puts the client on the lock's wait queue
retCode waitLock(lockPo l, double tmOut) {
#ifdef LOCKTRACE
  if (traceLock) {
    outMsg(logFile, RED_ESC_ON "waitLock on 0x%x"RED_ESC_OFF"\n%_", l);
  }
#endif
  retCode ret;

  if (pthread_mutex_lock(&l->mutex))  /* This will be transitory */
    syserr("problem in acquiring lock mutex");

  if ((pthread_equal(l->owner, pthread_self()) && l->count == 1) ||
      l->count == 0) {
    l->count = 0;
    l->owner = (pthread_t) Null;    /* the equivalent of unlocking */

    if (tmOut == 0.0) {      /* treat as a no-timeout wait */
#ifdef LOCKTRACE
      if (traceLock) {
        outMsg(logFile, RED_ESC_ON "waiting on 0x%x"RED_ESC_OFF"\n%_", l);
      }
#endif

      if (pthread_cond_broadcast(&l->cond))
        syserr("problem in broadcast");

      if (pthread_cond_wait(&l->cond, &l->mutex))
        syserr("problem in condwait");
      ret = Ok;
    } else {
      struct timespec tm;
      double seconds;
      double fraction = modf(tmOut, &seconds);

#ifdef LOCKTRACE
      if (traceLock)
        outMsg(logFile, RED_ESC_ON "waiting for %f seconds"RED_ESC_OFF"\n%_", seconds);
#endif
      tm.tv_sec = (long) seconds;
      tm.tv_nsec = (long) (fraction * NANO);  /* convert fractions to nanoseconds */

      int unixRet = 0;
      switch (unixRet = pthread_cond_timedwait(&l->cond, &l->mutex, &tm)) {
        case 0:
          ret = Ok;
          break;            /* somewhere else we will try to relock */
        case ETIMEDOUT:
          ret = Fail;
          break;
        default:
#ifdef LOCKTRACE
          if (traceLock)
            outMsg(logFile, RED_ESC_ON "error on wait %d"RED_ESC_OFF"\n%_", unixRet);
#endif

          ret = Error;
          break;
      }
    }
  } else {
#ifdef LOCKTRACE
    if (traceLock)
      outMsg(logFile, RED_ESC_ON "0x%x locked more than once"RED_ESC_OFF"\n%_", l);
#endif

    ret = Error; /* not allowed to wait on a lock that is locked more than once */
  }

  pthread_mutex_unlock(&l->mutex);
  return ret;
}

void initLock(lockPo l) {
  l->owner = (pthread_t) Null;
  l->count = 0;

  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
#ifdef TRACELOCK
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_ERRORCHECK);
#else
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_NORMAL);
#endif

  mtxInit:
  switch (pthread_mutex_init(&l->mutex, &attr)) {
    case 0:
      break;
    case EINVAL:
      syserr("cannot init");
    case ENOMEM:
      syserr("no memory");
    case EAGAIN:
      goto mtxInit;
    default:
      syserr("!!mutex init");
  }

  pthread_mutexattr_destroy(&attr);

  again:
  switch (pthread_cond_init(&l->cond, Null)) {
    case 0:
      return;
    case EAGAIN:
      goto again;
    default:
      syserr("cannot init lock");
      return;
  }
}
