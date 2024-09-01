/*
  Interval timer management
  Copyright (c) 2016, 2017 and beyond Francis G. McCabe
 */

#include "config.h"    /* pick up standard configuration header */
#include <signal.h>
#include <sys/time.h>
#include <sys/errno.h>
#include <math.h>
#include <pthread.h>
#include "utils.h"
#include "timer.h"
#include "pool.h"
#include "ooio.h"
#include <assert.h>

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
static retCode setInterrupt();

static void initAlarms(void) {
  if (timerPool == Null) {
    timerPool = newPool(sizeof(TimerStruct), 16);
    timeQ = Null;
    setInterrupt();
  }
}

static logical cmpTime(struct timeval *a, struct timeval *b) {
  if (a->tv_sec == b->tv_sec) {
    return a->tv_usec >= b->tv_usec;
  } else
    return a->tv_sec >= b->tv_sec;
}

retCode setInterrupt(){
  // Set up the signal handler
  struct sigaction act;
  act.sa_handler = timerWakeUp;
  act.sa_flags = 0;

 tryAgain:
  sigemptyset(&act.sa_mask);

  sigaction(SIGALRM, &act, Null);

  switch(errno){
  case 0:
    break;
  case EFAULT:
    outMsg(logFile,"EFAULT in sigaction\n%_");
    return Error;
  case EINVAL:
    outMsg(logFile,"EINVAL in sigaction\n%_");
    return Error;
  case EINTR:
    goto tryAgain;
  default:
    outMsg(logFile,"%d in segaction\n%_",errno);
    return Error;
  }     
  return Ok;
}
  
static retCode setupTimerInterrupt(struct timeval *when) {
  struct itimerval period;
  struct timeval now;

 tryAgain:
  gettimeofday(&now, Null);
  
  period.it_value.tv_sec = when->tv_sec - now.tv_sec;
  period.it_value.tv_usec = when->tv_usec - now.tv_usec;

  if (period.it_value.tv_usec < 0) {  /* -ve microseconds */
    period.it_value.tv_usec += MICROS;
    period.it_value.tv_sec--;
  }

  period.it_interval.tv_usec = 0;     // Single shot timer
  period.it_interval.tv_sec = 0;

  //  outMsg(logFile,"period secs: %d, micro %d\n%_",period.it_value.tv_sec,period.it_value.tv_usec);

  if(period.it_value.tv_sec<0 || period.it_value.tv_sec==0 && period.it_value.tv_usec<=0)
    return Interrupt;

  setitimer(ITIMER_REAL, &period, Null);

  switch(errno){
  case 0:
    return Ok;
  case EINVAL:{
    outMsg(logFile,"EINVAL in setitimer\n%_");
    return Error;
  }
  case EINTR:
    goto tryAgain;
  default:
    outMsg(logFile,"%d in setitimer\n%_",errno);
    return Error;
  }
}

static void clearTimerInterrupt() {
  struct itimerval zero = {.it_value={.tv_usec=0, .tv_sec=0}, .it_interval={.tv_sec=0, .tv_usec=0}};
  setitimer(ITIMER_REAL, &zero, Null);
}

static void processQ() {
  struct timeval now;

  tryAgain:
  gettimeofday(&now, Null);
  
  timePo q = timeQ;
  //  outMsg(logFile,"process timer Q %s \n%_",timeQLen(q));
  
  while(q != Null) {
    if(q->onWakeup!=Null){
      if (cmpTime(&now, &q->when)) {
        q->onWakeup(q->cl);
	q->onWakeup = Null;
      }
      else{
	switch(setupTimerInterrupt(&q->when)){
	case Ok:
	  return;
	case Interrupt:
	  goto tryAgain;
	case Error:
	default:
	  outMsg(logFile,"error in reset timer\n%_");
      }
      }
    }
    q = q->next;
  }
  clearTimerInterrupt(); // Only get here if no timers remain
}

static void timerWakeUp(int sig) {
  processQ(); // This does not modify the timerQ itself
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
  } else if(q->onWakeup==Null){
    timePo next = q->next;
    freePool(timerPool, q);
    return insertTimer(when,entry,next);
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
  processQ();
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
