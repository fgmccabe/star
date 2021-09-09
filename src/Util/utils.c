/*
  Utility functions
  Copyright (c) 2016, 2017. Francis G. McCabe
*/

#include <stdlib.h>
#include <sys/types.h>
#include <pwd.h>
#include "utils.h"
#include "formio.h"
#include "file.h"
#include "pool.h"

void syserr(const char *msg) {
  outMsg(logFile, "Fatal error: %s\n", msg);
  exit(99);
}

void check_(const char *func, const char *srcFile, int line, char *frag, char *msg) {
  logMsg(logFile, "assert %s failed (%s) at %s/%d in %s", frag, msg, srcFile, line, func);
  syserr("assert failed");
}

char *genSym(const char *prefix, char *buffer, integer buffLen) {
  static int count = 0;

  strMsg(buffer, buffLen, "%s%d", prefix, count++);
  return buffer;
}

retCode homeDir(char *user, char *buffer, integer bufLen) {
  struct passwd *up = getpwnam(user);
  if (up != Null) {
    uniCpy(buffer, bufLen, up->pw_dir);
    return Ok;
  }
  return Fail;
}

integer lg2(integer ix){
  uint64 v = (unsigned)ix;
  uint64 shift;
  uint64 r;

  r = (v>0xffffffff) << 5; v >>= r;
  shift = (v>0xffff) << 4; v >>= shift; r |=shift;
  shift = (v>0xff) << 3; v >>= shift; r |=shift;
  shift = (v>0xf) << 2; v >>= shift; r |=shift;
  shift = (v>0x3) << 1; v >>= shift; r |=shift;
  r |= (v>>1);

  return (integer)r;
}

static pthread_mutex_t prMutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_once_t prOnce = PTHREAD_ONCE_INIT;

/* Use the sieve of Erastosthenes to find the next prime that is larger
   than a given number
   Not the fastest algorithm, but it isnt called very often
*/


typedef struct primeHopper_ *primePo;

typedef struct primeHopper_ {
  integer pr;
  primePo next;
} primeHopper;

// Not too pretty, but hey who's going to see?

static poolPo prpool = NULL;
static primePo primes = NULL;

static void initPrimes(void) {
  if (prpool == NULL) {
    prpool = newPool(sizeof(primeHopper), 1024);
    primes = (primePo) allocPool(prpool);

    primes->pr = 3;      /* We know that 3 is a prime! */
    primes->next = NULL;
  }
}

integer nextPrime(integer min) {
  integer candidate = 0;

  pthread_once(&prOnce, initPrimes);
  pthread_mutex_lock(&prMutex);

  {
    primePo soFar = primes;

    while (candidate <= min && soFar != NULL) { // Look in the table we have so far
      candidate = soFar->pr;
      soFar = soFar->next;
    }

    while (candidate <= min) {  // We have to extend the table
      candidate += 2;

      again:
      for (soFar = primes; soFar != NULL; soFar = soFar->next) {
        if (candidate % soFar->pr == 0) {
          candidate += 2;
          goto again;
        } else if (soFar->next == NULL) {  // We have a new prime
          primePo new = (primePo) allocPool(prpool);

          new->pr = candidate;
          new->next = NULL;
          soFar->next = new;
          break;
        }
      }
    }
  }

  pthread_mutex_unlock(&prMutex);

  return candidate;
}
