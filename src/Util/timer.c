/*
  Interval timer management
  (c) 1994-2006 Imperial College and F.G. McCabe

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Contact: Francis McCabe <frankmccabe@mac.com>
 */
 
#include "config.h"		/* pick up standard configuration header */
#include <signal.h>
#include <time.h>
#include <math.h>
#include <pthread.h>
#include <stdlib.h>
#include <signal.h>
#include "number.h"
#include "retcode.h"
#include "timer.h"

static pthread_once_t once = PTHREAD_ONCE_INIT;
static pthread_key_t timerKey;

void initTimers(void)
{
  pthread_key_create(&timerKey,NULL);	/* create the timerKey */
}

typedef struct {
  timeFun onWakeup;
  void *cl;
} TimerStruct;

static void timerWakeUp(int sig)
{
  TimerStruct *ts = (TimerStruct*)pthread_getspecific(timerKey);

  pthread_setspecific(timerKey,NULL);

  if(ts!=NULL){
    if(ts->onWakeup!=NULL)
      ts->onWakeup(ts->cl);
    free(ts);
  }
}

retCode setAlarm(number time,timeFun onWakeup,void *cl)
{
  struct timeval now,when;

  pthread_once(&once,initTimers);

  gettimeofday(&now, NULL);
  when.tv_sec=time;
  when.tv_usec = (time - floor(time)) * 1000000;

  if(when.tv_sec<now.tv_sec ||
     (when.tv_sec==now.tv_sec && when.tv_usec<now.tv_usec)) /* already gone */
    return Fail;			/* timeout already fired */
  else{
    struct itimerval period;
    struct sigaction act;

    period.it_value.tv_sec = when.tv_sec - now.tv_sec;
    period.it_value.tv_usec = when.tv_usec - now.tv_usec;

    if(period.it_value.tv_usec < 0){	/* -ve microseconds */
      period.it_value.tv_usec += 1000000;
      period.it_value.tv_sec--;
    }

    period.it_interval.tv_sec = period.it_interval.tv_usec = 0;

    if(onWakeup!=NULL){
      TimerStruct *ts = (TimerStruct*)malloc(sizeof(TimerStruct));
      pthread_setspecific(timerKey,ts);

      ts -> onWakeup = onWakeup;
      ts -> cl = cl;
    }

    act.sa_handler = timerWakeUp;
    act.sa_flags = 0;
  
    sigemptyset(&act.sa_mask);

    sigaction(SIGALRM,&act,NULL);
    setitimer(ITIMER_REAL, &period, NULL);
    return Fail;
  }
}

void cancelAlarm(void)
{
  struct itimerval period;
  struct sigaction act;

  TimerStruct *ts = (TimerStruct*)pthread_getspecific(timerKey);

  if(ts!=NULL){
    free(ts);
    pthread_setspecific(timerKey,NULL);
  }

  period.it_value.tv_sec = period.it_value.tv_usec = 0;
  period.it_interval.tv_sec = period.it_interval.tv_usec = 0;
  setitimer(ITIMER_REAL,&period,NULL);	/* Cancel the timer (if set) */

  act.sa_handler = SIG_IGN;
  act.sa_flags = 0;

  sigaction(SIGALRM,&act,NULL);
}
