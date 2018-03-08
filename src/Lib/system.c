/*
 * Some system functions
 */
#include <sys/time.h>
#include <errno.h>
#include <stdlib.h>
#include "engine.h"
#include "arith.h"


// Number of nano seconds
#define NANOS 1000000000


void sleep(integer amnt) {
  long nanos = amnt % NANOS;
  long secs = amnt / NANOS;
  struct timespec Amnt = {.tv_sec=secs, .tv_nsec=nanos};
  struct timespec SoFar;

  while (nanosleep(&Amnt, &SoFar) != 0) {
    switch (errno) {
      case ENOSYS:
        outMsg(logFile, "no nanosleep\n");
        exit(99);
      case EINTR:
        if (SoFar.tv_sec != 0 || SoFar.tv_nsec != 0)
          Amnt = SoFar;
        else
          return;
      default:
        outMsg(logFile, "problem in nanosleep");
        exit(99);
    }
  }
}

/* Fatal system error */
void syserr(const char *msg) {
  outMsg(logFile, "Fatal error: %U\n", msg);
  exit(99);
}

void memerr() {
  outMsg(logFile, "out of heap space\n");
  flushOut();
  exit(99);
}

ReturnStatus g__exit(processPo p, ptrPo tos){
  integer ix = integerVal(tos[0]);

  exit((int)ix);
}
