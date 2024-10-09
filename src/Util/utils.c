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
#include "hash.h"

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

integer lg2(integer ix) {
  uint64 v = (uint64) ix;
  uint64 shift;
  uint64 r;

  r = (v > 0xffffffffu) << 5;
  v >>= r;
  shift = (v > 0xffffu) << 4;
  v >>= shift;
  r |= shift;
  shift = (v > 0xffu) << 3;
  v >>= shift;
  r |= shift;
  shift = (v > 0xfu) << 2;
  v >>= shift;
  r |= shift;
  shift = (v > 0x3u) << 1;
  v >>= shift;
  r |= shift;
  r |= (v >> 1u);

  return (integer) r;
}

integer lg2Ceiling(integer ix) {
  return lg2(ix * 2 - 1);
}

integer countBits(integer ix) {
  uinteger ux = (unsigned) ix;

  static uinteger SK5 = 0x5555555555555555;
  static uinteger SK3 = 0x3333333333333333;
  static uinteger SKF0 = 0x0F0F0F0F0F0F0F0F;
  static uinteger SKFF = 0x00FF00FF00FF00FF;
  static uinteger SKFF16 = 0x0000FFFF0000FFFF;
  static uinteger SKFF32 = 0x00000000FFFFFFFF;

  ux = (ux & SK5) + ((ux >> 1u) & SK5);
  ux = (ux & SK3) + ((ux >> 2u) & SK3);
  ux = (ux & SKF0) + ((ux >> 4u) & SKF0);
  ux = (ux & SKFF) + ((ux >> 8u) & SKFF);
  ux = (ux & SKFF16) + ((ux >> 16u) & SKFF16);
  ux = (ux & SKFF32) + ((ux >> 32u) & SKFF32);
  return (integer) ux;
}

integer intGCD(integer a, integer b) {
  a = absolute(a);
  b = absolute(b);
  do {
    if (b > a) {
      integer w = a;
      a = b;
      b = w;
    }
    if (a > b) {
      integer r = a % b;
      if (r == 0) {
        return b;
      } else
        a = r;
    }
  } while (a != b);
  return a;
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

// Manage hwms

integer hwmOf(hwmPo hwm){
  return hwm->max;
}

integer hwmBump(hwmPo hwm, integer delta){
  hwm->current+=delta;
  if(hwm->current>hwm->max)
    hwm->max = hwm->current;
  return hwm->current;
}
