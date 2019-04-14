/*
  Utility functions
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include <stdlib.h>
#include "utils.h"
#include "formio.h"
#include "file.h"
#include "pool.h"

void syserr(const char *msg) {
  outMsg(logFile, "Fatal error: %s\n", msg);
  exit(99);
}

char *genSym(char *prefix) {
  static int count = 0;
  static char buffer[MAXLINE];

  strMsg(buffer, NumberOf(buffer), "%s%d", prefix, count++);
  return buffer;
}

static pthread_mutex_t prMutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_once_t prOnce = PTHREAD_ONCE_INIT;

/* Use the sieve of Erastosthenes to find the next prime that is larger
   than a given number
   Not the fastest algorithm, but it isnt called very often
*/


typedef struct _primeHopper_ *primePo;

typedef struct _primeHopper_ {
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

char *stripFileScheme(char *src){
  if(uniIsLitPrefix(src,"file:"))
    return src+uniStrLen("file:");
  else
    return src;
}
