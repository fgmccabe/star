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

static int time_offset()
{
  time_t gmt, rawtime = time(NULL);
  struct tm *ptm;

  struct tm gbuf;
  ptm = gmtime_r(&rawtime, &gbuf);

  // Request that mktime() lookup dst in timezone database
  ptm->tm_isdst = -1;
  gmt = mktime(ptm);

  return (int)difftime(rawtime, gmt);
}

void initTime(void) {
  timezone_offset = time_offset();
}

/*
 * reset the interval timer for the new period
 */

ValueReturn s__delay(enginePo P,termPo d) {
  double dx = floatVal(d);

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
	return abnormalReturn(eINTRUPT);
      }
      case EINVAL:
      case ENOSYS:
      default: {
	return abnormalReturn(eINVAL);
      }
    }
  } else {
//    setProcessRunnable(p);
    return normalReturn(unitEnum);
  }
}

ReturnStatus g__delay(enginePo P) {
  termPo d = popVal(P);
  ValueReturn ret = s__delay(P,d);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__sleep(enginePo P,termPo d) {
  double f = floatVal(d);

  struct timeval now;
  double seconds;
  double fraction = modf(f, &seconds);

  gettimeofday(&now, NULL);

  if ((long) seconds < now.tv_sec ||
      ((long) seconds == now.tv_sec && (fraction * 1000000) < now.tv_usec)) {
    return normalReturn(unitEnum);
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
	  return abnormalReturn(eINTRUPT);
        }
        case EINVAL:
        case ENOSYS:
        default: {
	  return abnormalReturn(eINVAL);
        }
      }
    } else {
//      setProcessRunnable(p);
      return normalReturn(unitEnum);
    }
  }
}

ReturnStatus g__sleep(enginePo P) {
  termPo d = popVal(P);
  ValueReturn ret = s__sleep(P,d);
  pshVal(P,ret.value);
  return ret.status;
}

/* Return the current time */
ValueReturn s__now(enginePo P){
  return normalReturn(makeFloat(get_time()));
}

ReturnStatus g__now(enginePo P) {
  ValueReturn ret = s__now(P);
  pshVal(P,ret.value);
  return ret.status;
}

/* Return the time at midnight */
ValueReturn s__today(enginePo P){
  return normalReturn(makeFloat(get_date()));
}
  
ReturnStatus g__today(enginePo P) {
  ValueReturn ret = s__today(P);
  pshVal(P,ret.value);
  return ret.status;
}

ValueReturn s__ticks(enginePo P){
  return normalReturn(makeInteger((integer)clock()));
}
  
ReturnStatus g__ticks(enginePo P) {
  ValueReturn ret = s__ticks(P);
  pshVal(P,ret.value);
  return ret.status;
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
    tmptr->tm_gmtoff = timezone_offset;

    return (double) mktime(tmptr);
  } else
    return (double) NAN;
}
