/*
  Clock and interval timer management for the Star system
  Copyright (c) 2016 and beyond. Francis G. McCabe
 */

#include <time.h>
#include <math.h>
#include <errno.h>
#include "ooio.h"
#include "clock.h"
#include "engine.h"
#include "arithP.h"
#include "errorCodes.h"
#include "globals.h"
#include "stack.h"
#include "escape.h"

long timezone_offset;    // offset in seconds from GMT

void initTime(void) {
  struct timeval initial_time;    // Time when the engine started
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

ReturnStatus g__delay(processPo P) {
  double dx = floatVal(popVal(P));

  struct timespec tm;
  double seconds;
  double fraction = modf(dx, &seconds);

#define NANO (1000000000)

  tm.tv_sec = (long) seconds;
  tm.tv_nsec = (long) (fraction * NANO);  /* Convert microseconds to nanoseconds */
//  switchProcessState(p, wait_timer);
  if (nanosleep(&tm, NULL) != 0) {
//    setProcessRunnable(p);
    switch (errno) {
      case EINTR: {
        pshVal(P, eINTRUPT);
        return Abnormal;
      }
      case EINVAL:
      case ENOSYS:
      default: {
        pshVal(P, eINVAL);
        return Abnormal;
      }
    }
  } else {
//    setProcessRunnable(p);
    pshVal(P, unitEnum);
    return Normal;
  }
}

ReturnStatus g__sleep(processPo P) {
  double f = floatVal(popVal(P));

  struct timeval now;
  double seconds;
  double fraction = modf(f, &seconds);

  gettimeofday(&now, NULL);

  if ((long) seconds < now.tv_sec ||
      ((long) seconds == now.tv_sec && (fraction * 1000000) < now.tv_usec)) {
    pshVal(P, unitEnum);
    return Normal;
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

//    switchProcessState(p, wait_timer);
    if (nanosleep(&tm, NULL) != 0) {
//      setProcessRunnable(p);
      switch (errno) {
        case EINTR: {
          pshVal(P, eINTRUPT);
          return Abnormal;
        }
        case EINVAL:
        case ENOSYS:
        default: {
          pshVal(P, eINVAL);
          return Abnormal;
        }
      }
    } else {
//      setProcessRunnable(p);
      pshVal(P, unitEnum);
      return Normal;
    }
  }
}

/* Return the current time */
ReturnStatus g__now(processPo P) {
  pshVal(P, makeFloat(get_time()));
  return Normal;
}

/* Return the time at midnight */
ReturnStatus g__today(processPo P) {
  pshVal(P, makeFloat(get_date()));
  return Normal;
}

ReturnStatus g__ticks(processPo P) {
  pshVal(P, makeInteger((integer) clock()));
  return Normal;
}

/*
 *  returns the current time
 */
double get_time(void) {
  struct timeval t;

  gettimeofday(&t, NULL);

  return (double) t.tv_sec + t.tv_usec / 1.0e6;
}

/*
 *  returns the time at midnight this morning
 */
double get_date(void) {
  struct timeval t;
  time_t tloc;
  struct tm *tmptr;

  if (gettimeofday(&t, NULL) == 0) {
    tloc = t.tv_sec;
    tmptr = localtime(&tloc);
    tmptr->tm_hour = 0;
    tmptr->tm_min = 0;
    tmptr->tm_sec = 0;

    return (double) mktime(tmptr);
  } else
    return (double) NAN;
}
