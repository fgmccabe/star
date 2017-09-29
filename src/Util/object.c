/*
  Object base class
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
#include <stdio.h>
#include <stdarg.h>
#include <pthread.h>
#include "objectP.h"

/* Object base implementation class */

static void objectClassInit(classPo class, classPo request);

static objectPo objectCreate(classPo class);

static void objectDestroy(objectPo o);

static void objectErase(objectPo o);

static void objectInit(objectPo o, va_list *args);

static integer objectHash(objectPo o);

static logical objectEquality(objectPo o1, objectPo o2);

ObjectClassRec ObjectClass = {
  NULL,                                 /* has no parent class */
  "object",
  NULL,
  objectClassInit,                      /* standard class initializer */
  objectCreate,                         /* object creation */
  objectDestroy,                        /* object destruction */
  objectErase,                          /* object removal */
  objectInit,                           /* object initialization */
  sizeof(ObjectRec),                    /* Each object's minimum size */
  objectHash,                           // Default has function for objects
  objectEquality,                       // Default equality function for objects
  NULL,          /* pool of values for this class */
  PTHREAD_ONCE_INIT,      /* not initialized yet */
  PTHREAD_MUTEX_INITIALIZER,    /* class lock */
};

classPo objClass = &ObjectClass;

static void objectClassInit(classPo class, classPo request) {
  request->pool = newPool(request->size, 8);
  if (request->create == O_INHERIT_DEF)
    request->create = class->create;
  if (request->erase == O_INHERIT_DEF)
    request->erase = class->erase;
  if (request->hashCode == O_INHERIT_DEF)
    request->hashCode = class->hashCode;
  if (request->equality == O_INHERIT_DEF)
    request->equality = class->equality;
  initRecursiveMutex(&request->mutex);
}

/* Generic object create function */
static objectPo objectCreate(classPo class) {
  objectPo o = (objectPo) allocPool(class->pool);

  o->class = class;
  o->refCount = 1;
  return o;
}

long referenceCount(objectPo o) {
  return o->refCount;
}

void incReference(objectPo o) {
  o->refCount++;
}

void decReference(objectPo o) {
  long count = o->refCount--;
  if (count == 0) {
    destroyObject(o);
  }
}

void initRecursiveMutex(pthread_mutex_t *mutex) {
  pthread_mutexattr_t attr;

  pthread_mutexattr_init(&attr);
  pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);

  pthread_mutex_init(mutex, &attr);
  pthread_mutexattr_destroy(&attr);
}

static void objectInit(objectPo o, va_list *args) {
  o->refCount = 1;
}

static void objectDestroy(objectPo o) {
}

/* Generic object erasure function */
static void objectErase(objectPo o) {
  freePool(o->class->pool, o);
}

integer hashCode(objectPo o) {
  return o->class->hashCode(o);
}

logical equals(objectPo o1, objectPo o2) {
  if (o1->class == o2->class) {
    return o1->class->equality(o1, o2);
  } else
    return False;
}
// Implement default hashcode and equality functions

static integer objectHash(objectPo o) {
  return (integer) o;
}

static logical objectEquality(objectPo o1, objectPo o2) {
  if (o1 == o2)
    return True;
  else
    return False;
}

void lockClass(classPo c) {
  pthread_mutex_lock(&c->mutex);
}

void unlockClass(classPo c) {
  pthread_mutex_unlock(&c->mutex);
}


