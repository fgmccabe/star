//
// Created by Francis McCabe on 4/16/23.
//

#include "integer.h"
#include "timers.h"
#include "pool.h"
#include <clock.h>
#include <assert.h>
#include "ooio.h"

logical enableTimers = False;

typedef struct timer_record {
  char *name;
  integer current;
  integer total;
  timerPo prev;
} TimerRecord;

static poolPo timerPool;
static timerPo timers = Null;

void initTimers() {
  timerPool = newPool(sizeof(TimerRecord), 128);
}

timerPo startTimer_(char *msg, logical running) {
  timerPo timer = (timerPo) allocPool(timerPool);
  timer->total = 0;
  timer->current = (running ? (integer) clock() : 0);
  timer->name = msg;
  timer->prev = timers;
  timers = timer;
  return timer;
}

void pauseTimer_(timerPo timer) {
  assert(timer->current != 0);

  timer->total += ((integer) clock()) - timer->current;
  timer->current = 0;
}

void resumeTimer_(timerPo timer) {
  assert(timer->current == 0);

  timer->current = (integer) clock();
}

logical isTimerRunning(timerPo timer) {
  return timer != Null && timer->current != 0;
}

void reportTimers() {
  integer total = 0;

  for (timerPo tmr = timers; tmr != Null; tmr = tmr->prev) {
    if (tmr->current != 0)
      pauseTimer(tmr);
    total += tmr->total;
  }

  for (timerPo tmr = timers; tmr != Null; tmr = tmr->prev) {
    if (tmr->total > 0) {
      outMsg(logFile, "Timer %s: %ld %g\n", tmr->name, tmr->total, ((double) tmr->total / total) * 100.0);
    }
  }

  if (total > 0)
    outMsg(logFile, "Total time: %ld\n", total);
}
