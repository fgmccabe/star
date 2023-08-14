/*
  Lockable objects - implementation
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "lockableP.h"
#include <math.h>
#include <errno.h>
#include "timer.h"

static void lockedObjectInit(objectPo o, va_list *args);

static void lockedDestroy(objectPo o);

LockClassRec LockedClass = {
  {
    (classPo) &ObjectClass,
    "locked",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    lockedDestroy,
    O_INHERIT_DEF,
    lockedObjectInit,
    sizeof(LockedRecord),
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo lockedClass = (classPo) &LockedClass;

void lockedObjectInit(objectPo o, va_list *args) {
  lockedPo l = O_LOCKED(o);

  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);

  pthread_mutex_init(&l->lock.mutex, &attr);
  pthread_mutexattr_destroy(&attr);
}

void lockedDestroy(objectPo o) {
  lockedPo l = O_LOCKED(o);
  pthread_mutex_destroy(&l->lock.mutex);
}

// Implement the object lock and release functions

void lock(lockedPo l) {
  pthread_mutex_lock(&l->lock.mutex);
}

void unlock(lockedPo l) {
  pthread_mutex_unlock(&l->lock.mutex);
}

