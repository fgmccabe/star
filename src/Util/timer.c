/*
  Interval timer management
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
 */

#include "config.h"		/* pick up standard configuration header */
#include <signal.h>
#include <time.h>
#include <math.h>
#include <pthread.h>
#include <stdlib.h>
#include "retcode.h"
#include "timer.h"

static pthread_once_t once = PTHREAD_ONCE_INIT;
static pthread_key_t timerKey;

void initTimers(void) {
  pthread_key_create(&timerKey, NULL);  /* create the timerKey */
}

typedef struct {
  timeFun onWakeup;
  void *cl;
} TimerStruct;

static void timerWakeUp(int sig) {
  TimerStruct *ts = (TimerStruct *) pthread_getspecific(timerKey);

  pthread_setspecific(timerKey, NULL);

  if (ts != NULL) {
    if (ts->onWakeup != NULL)
      ts->onWakeup(ts->cl);
    free(ts);
  }
}

retCode setAlarm(double time, timeFun onWakeup, void *cl) {
  struct timeval now, when;

  pthread_once(&once, initTimers);

  gettimeofday(&now, NULL);
  when.tv_sec = (time_t)floor(time);
  when.tv_usec = (int32)((time - floor(time)) * 1000000);

  if (when.tv_sec < now.tv_sec ||
      (when.tv_sec == now.tv_sec && when.tv_usec < now.tv_usec)) /* already gone */
    return Fail;      /* timeout already fired */
  else {
    struct itimerval period;
    struct sigaction act;

    period.it_value.tv_sec = when.tv_sec - now.tv_sec;
    period.it_value.tv_usec = when.tv_usec - now.tv_usec;

    if (period.it_value.tv_usec < 0) {  /* -ve microseconds */
      period.it_value.tv_usec += 1000000;
      period.it_value.tv_sec--;
    }

    period.it_interval.tv_sec = period.it_interval.tv_usec = 0;

    if (onWakeup != NULL) {
      TimerStruct *ts = (TimerStruct *) malloc(sizeof(TimerStruct));
      pthread_setspecific(timerKey, ts);

      ts->onWakeup = onWakeup;
      ts->cl = cl;
    }

    act.sa_handler = timerWakeUp;
    act.sa_flags = 0;

    sigemptyset(&act.sa_mask);

    sigaction(SIGALRM, &act, NULL);
    setitimer(ITIMER_REAL, &period, NULL);
    return Ok;
  }
}

void cancelAlarm(void) {
  struct itimerval period;
  struct sigaction act;

  TimerStruct *ts = (TimerStruct *) pthread_getspecific(timerKey);

  if (ts != NULL) {
    free(ts);
    pthread_setspecific(timerKey, NULL);
  }

  period.it_value.tv_sec = period.it_value.tv_usec = 0;
  period.it_interval.tv_sec = period.it_interval.tv_usec = 0;
  setitimer(ITIMER_REAL, &period, NULL);  /* Cancel the timer (if set) */

  act.sa_handler = SIG_IGN;
  act.sa_flags = 0;

  sigaction(SIGALRM, &act, NULL);
}
