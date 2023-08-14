/*
  Interval timer management
  Copyright (c) 2016, 2017 and beyond Francis G. McCabe
 */

#include "config.h"    /* pick up standard configuration header */
#include <signal.h>
#include <sys/time.h>
#include <math.h>
#include <pthread.h>
#include "utils.h"
#include "timer.h"
#include "pool.h"

static poolPo timerPool = Null;

typedef struct timer_struct *timePo;

typedef struct timer_struct {
  struct timeval when;
  timeFun onWakeup;
  void *cl;
  timePo next;
} TimerStruct;

static pthread_once_t once = PTHREAD_ONCE_INIT;
static timePo timeQ = Null;
static void timerWakeUp(int sig);

static void initAlarms(void) {
  if (timerPool == Null) {
    timerPool = newPool(sizeof(TimerStruct), 16);
    timeQ = Null;
  }
}

static logical cmpTime(struct timeval *a, struct timeval *b) {
  if (a->tv_sec == b->tv_sec) {
    return a->tv_usec >= b->tv_usec;
  } else
    return a->tv_sec >= b->tv_sec;
}

static void setupTimerInterrupt(struct timeval *when, struct timeval *now) {
  struct itimerval period;

  period.it_value.tv_sec = when->tv_sec - now->tv_sec;
  period.it_value.tv_usec = when->tv_usec - now->tv_usec;

  if (period.it_value.tv_usec < 0) {  /* -ve microseconds */
    period.it_value.tv_usec += MICROS;
    period.it_value.tv_sec--;
  }
  period.it_interval.tv_usec = 0;     // Single shot timer
  period.it_interval.tv_sec = 0;

  setitimer(ITIMER_REAL, &period, Null);
  // Set up the signal handler
  struct sigaction act;
  act.sa_handler = timerWakeUp;
  act.sa_flags = 0;

  sigemptyset(&act.sa_mask);

  sigaction(SIGALRM, &act, Null);
}

static void clearTimerInterrupt() {
  struct itimerval zero = {.it_value={.tv_usec=0, .tv_sec=0}, .it_interval={.tv_sec=0, .tv_usec=0}};
  setitimer(ITIMER_REAL, &zero, Null);
}

static timePo processQ(timePo q, struct timeval *now) {
  if (q != Null) {
    if (cmpTime(now, &q->when)) {
      if (q->onWakeup != Null) {
        q->onWakeup(q->cl);
      }
      timePo next = q->next;
      freePool(timerPool, q);
      return processQ(next, now);
    } else {
      setupTimerInterrupt(&q->when, now);

      return q;
    }
  } else {
    clearTimerInterrupt();
    return Null;
  }
}

static void timerWakeUp(int sig) {
  struct timeval now;
  gettimeofday(&now, Null);
  timeQ = processQ(timeQ, &now);
}

retCode setupAlarm(struct timeval when, timeFun onWakeup, void *cl);

retCode setAlarm(double time, timeFun onWakeup, void *cl) {
  struct timeval when;

  when.tv_sec = (time_t) floor(time);
  when.tv_usec = (int32) ((time - floor(time)) * MICROS);

  return setupAlarm(when, onWakeup, cl);
}

static timePo insertTimer(struct timeval *when, timePo entry, timePo q) {
  if (q == Null) {
    entry->next = Null;
    return entry;
  } else if (cmpTime(&q->when, when)) { // new one is younger
    entry->next = q;
    return entry;
  } else {
    q->next = insertTimer(when, entry, q->next);
    return q;
  }
}

retCode setupAlarm(struct timeval when, timeFun onWakeup, void *cl) {
  pthread_once(&once, initAlarms);

  timePo entry = (timePo) allocPool(timerPool);

  entry->when = when;
  entry->onWakeup = onWakeup;
  entry->cl = cl;
  entry->next = Null;

  timeQ = insertTimer(&when, entry, timeQ);
  struct timeval now;
  gettimeofday(&now, Null);
  timeQ = processQ(timeQ, &now);
  return Ok;
}

retCode setTimer(double time, timeFun onWakeup, void *cl) {
  struct timeval when;
  gettimeofday(&when, Null);

  integer micros = (integer) ((time - floor(time)) * MICROS);
  if (((integer) when.tv_usec) + micros > MICROS) {
    when.tv_usec = (int) ((when.tv_usec + micros) - MICROS);
    when.tv_sec += (int) floor(time) + 1;
  } else {
    when.tv_usec += (int) micros;
    when.tv_sec += (int) floor(time);
  }

  return setupAlarm(when, onWakeup, cl);
}
