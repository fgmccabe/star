/*
  Object base class private header
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/

#include "config.h"
#include "object.h"
#include "pool.h"
#include <stdarg.h>

#ifndef _OBJECT_P_H_
#define _OBJECT_P_H_

typedef void (*objectProc)(objectPo o);
typedef objectPo (*objectCreateProc)(classPo cl);
typedef void (*objectClassProc)(classPo class);
typedef void (*classInitProc)(classPo class,classPo request);
typedef integer (*hashProc)(objectPo o);
typedef logical (*equalityProc)(objectPo o,objectPo p);

#ifndef O_INHERIT_DEF
#define O_INHERIT_DEF ((void*)-1)
#endif

typedef struct _object_ {
  classPo class;                        /* class of the object */
  int32 refCount;                       /* reference count of object */
} ObjectRec;

typedef struct _class_ {
  classPo parent;                       /* parent class of this object */
  char *className;                      /* name of this class */
  classInitProc classInit;            /* procedure to initialize classes */
  objectCreateProc create;              /* procedure to create an object */
  objectProc destroy;                   /* procedure to destroy an object */
  objectProc erase;                    /* procedure to remove object's memory */
  void (*init)(objectPo o,va_list *args); /* procedure to initialize an object */
  size_t size;                            /* size of an individual object */
  hashProc hashCode;                      // Hashcode function for this class
  equalityProc equality;                  // Equality function for this class
  poolPo pool;                            // What pool is sued for this class?
  pthread_once_t inited;		/* This flag controls initialization */
  pthread_mutex_t mutex;		/* This allows a class-level lock */
} ObjectClassRec;

extern ObjectClassRec ObjectClass;

objectPo checkCast(void *c,classPo class);

#ifdef ALLTRACE
#ifndef VERIFY_OBJECT
#define VERIFY_OBJECT
#endif
#endif
#endif
