/*
  Clock and interval timer management for the Star system
  Copyright (c) 2016 and beyond. Francis G. McCabe
 */

#include <signal.h>
#include <time.h>
#include <math.h>
#include <errno.h>
#include "ooio.h"
#include "clock.h"
#include "engine.h"
#include "arithP.h"
#include "errorCodes.h"
#include "globals.h"

long timezone_offset;    // offset in seconds from GMT

struct timeval initial_time;    // Time when the engine started

void initTime(void) {
  time_t tloc;
  struct tm *tmptr;

  gettimeofday(&initial_time, NULL);

  tloc = initial_time.tv_sec;
  tmptr = localtime(&tloc);
  tmptr->tm_hour = 0;
  tmptr->tm_min = 0;
  tmptr->tm_sec = 0;

  timezone_offset = mktime(tmptr) - (tloc - tloc % SECSINDAY);
}

/*
 * reset the interval timer for the new period
 */

ReturnStatus g__delay(processPo p, termPo xc, termPo a1) {
  double dx = floatVal(a1);

  struct timespec tm;
  double seconds;
  double fraction = modf(dx, &seconds);

#define NANO (1000000000)

  tm.tv_sec = (long) seconds;
  tm.tv_nsec = (long) (fraction * NANO);  /* Convert microseconds to nanoseconds */
  switchProcessState(p, wait_timer);
  if (nanosleep(&tm, NULL) != 0) {
    setProcessRunnable(p);
    switch (errno) {
      case EINTR:
        return (ReturnStatus) {.ret = Abnormal, .cont = xc, .result = eINTRUPT};
      case EINVAL:
      case ENOSYS:
      default:
        return (ReturnStatus) {.ret = Abnormal, .cont = xc, .result = eINVAL};
    }
  } else {
    setProcessRunnable(p);
    return (ReturnStatus) {.ret = Normal, .result = unitEnum};
  }
}

ReturnStatus g__sleep(processPo p, termPo xc, termPo a1) {
  double f = floatVal(a1);

  struct timeval now;
  double seconds;
  double fraction = modf(f, &seconds);

  gettimeofday(&now, NULL);

  if (seconds < now.tv_sec ||
      (seconds == now.tv_sec && (fraction * 1000000) < now.tv_usec)) {
    return (ReturnStatus) {.ret = Normal, .result = unitEnum};
  } else {
    struct timespec tm;

    tm.tv_sec = (long) seconds;
    tm.tv_nsec = (long) (fraction * NANO);  /* Convert microseconds to nanoseconds */

    tm.tv_sec = (long) seconds - now.tv_sec;
    tm.tv_nsec = (long) (fraction * NANO) - now.tv_usec * 1000; /* Convert microseconds to nanoseconds */
    if (tm.tv_nsec > NANO) {
      tm.tv_nsec -= NANO;
      tm.tv_sec++;
    } else if (tm.tv_nsec < 0) {
      tm.tv_nsec += NANO;
      tm.tv_sec--;
    }

    switchProcessState(p, wait_timer);
    if (nanosleep(&tm, NULL) != 0) {
      setProcessRunnable(p);
      switch (errno) {
        case EINTR:
          return (ReturnStatus) {.ret = Abnormal, .cont = xc, .result = eINTRUPT};
        case EINVAL:
        case ENOSYS:
        default:
          return (ReturnStatus) {.ret = Abnormal, .cont = xc, .result = eINVAL};
      }
    } else {
      setProcessRunnable(p);
      return (ReturnStatus) {.ret = Normal, .result = unitEnum};
    }
  }
}

/* Return the current time */
ReturnStatus g__now(heapPo h) {
  termPo now = makeFloat(get_time());

  return (ReturnStatus) {.ret=Normal, .result=now};
}

/* Return the time at midnight */
ReturnStatus g__today(heapPo h) {
  termPo now = makeFloat(get_date());

  return (ReturnStatus) {.ret=Normal, .result=now};
}

ReturnStatus g__ticks(heapPo h) {
  termPo now = makeInteger((integer) clock());

  return (ReturnStatus) {.ret=Normal, .result=now};
}

/*
 *  returns the current time
 */
double get_time(void) {
  struct timeval t;

  gettimeofday(&t, NULL);

  return t.tv_sec + t.tv_usec / 1.0e6;
}

/*
 *  returns the time at midnight this morning
 */
double get_date(void) {
  struct timeval t;

  gettimeofday(&t, NULL);

  t.tv_sec -= t.tv_sec % SECSINDAY;
  return (double) (t.tv_sec + timezone_offset);
}
