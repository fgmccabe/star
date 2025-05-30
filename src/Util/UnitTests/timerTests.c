//
// Created by Francis McCabe on 8/13/23.
//
#include "timertests.h"

#include <assert.h>
#include <sys/time.h>
#include "math.h"

static void setupTests() {
}

static void tearDownTests() {
}

static void addDelta(struct timeval *when, double delta) {
  int deltaMicro = (int) ((delta - floor(delta)) * MICROS);
  int deltaSecs = (int) floor(delta);

  if (((integer) when->tv_usec) + deltaMicro > MICROS) {
    when->tv_usec = (int) ((when->tv_usec + deltaMicro) - MICROS);
    when->tv_sec += deltaSecs + 1;
  } else {
    when->tv_usec += deltaMicro;
    when->tv_sec += deltaSecs;
  }
}

static logical cmpTime(struct timeval *a, struct timeval *b) {
  if (a->tv_sec == b->tv_sec) {
    return a->tv_usec >= b->tv_usec;
  } else
    return a->tv_sec >= b->tv_sec;
}

static void ping(logical *done) {
  *done = True;
}

retCode singleTimerTest() {
  if (debugUnitTests)
    outMsg(logFile, "single timer\n%_");

  struct timeval when, next;
  gettimeofday(&when, Null);

  logical done = False;

  tryRet(setTimer(0.0001, (timeFun) ping, &done));

  while (!done)
    outMsg(logFile, "%\r%_");

  gettimeofday(&next, Null);
  return boolRet(cmpTime(&next, &when));
}

static void ascendCheck(struct timeval *last) {
  outMsg(logFile, "woke up\n%_");
  struct timeval now;
  gettimeofday(&now, Null);

  assert(cmpTime(&now, last));
  *last = now;
}

static void tCheck(integer ix) {
  outMsg(logFile, "woke up %ld\n%_", ix);
}

retCode ascendingTimerTest() {
  outMsg(logFile, "ascending sequence timer\n%_");

  struct timeval start, next;
  gettimeofday(&start, Null);
  next = start;

  for (integer ix = 0; ix < 100; ix++) {
    // tryRet(setTimer((double)ix * 0.001, (timeFun) ascendCheck, &next));
    tryRet(setTimer((double) ix * 0.001, (timeFun) tCheck, (void *) ix));
  }

  logical done = False;

  tryRet(setTimer(1.0, (timeFun) ping, &done));

  while (!done);
//    outMsg(logFile, "&\r%_");

  gettimeofday(&next, Null);
  return boolRet(cmpTime(&next, &start));
}

retCode descendingTimerTest() {
  outMsg(logFile, "descending sequence timer\n%_");

  struct timeval start, next;
  gettimeofday(&start, Null);
  next = start;

  for (integer ix = 100; ix > 0; ix--) {
//    tryRet(setTimer(ix * 0.001, (timeFun) ascendCheck, &next));
    tryRet(setTimer((double)ix * 0.001, (timeFun) tCheck, (void*)ix));
  }

  logical done = False;

  setTimer(1.0, (timeFun) ping, &done);

  while (!done);
//    outMsg(logFile, "*\r%_");

  gettimeofday(&next, Null);
  return boolRet(cmpTime(&next, &start));
}

retCode timerTests() {
  setupTests();
  tryRet(run_test(singleTimerTest));
  tryRet(run_test(ascendingTimerTest));
  tryRet(run_test(descendingTimerTest));
  tearDownTests();
  return Ok;
}

retCode all_tests() {
  return timerTests();
}
