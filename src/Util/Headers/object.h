#ifndef _OBJECT_H_
#define _OBJECT_H_

/*
  Public definition of object
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
#include "logical.h"
#include "retcode.h"
#include "integer.h"
#include "utils.h"
#include <unistd.h>
#include <pthread.h>

typedef struct _object_ *objectPo;
typedef struct _class_ *classPo;

extern classPo objClass;

objectPo newObject(classPo c,...);
void destroyObject(objectPo o);

char *nameOfClass(classPo class);
logical isSubClass(classPo class,classPo parent);
classPo parentClass(classPo class);

classPo classOfObject(objectPo o);
logical objectHasClass(objectPo o, classPo parent);
logical isObject(objectPo o);

integer hashCode(objectPo o);
logical equals(objectPo o1,objectPo o2);

void lockClass(classPo c);
void unlockClass(classPo c);

// A convenience hack
void initRecursiveMutex(pthread_mutex_t *mutex);

// Reference count memory management
void incReference(objectPo o);
void decReference(objectPo o);
long referenceCount(objectPo o);

#ifdef VERIFY_OBJECT
#define O_OBJECT(c) ((objectPo)(checkCast((c),objClass)))
#else
#define O_OBJECT(c) ((objectPo)(c))
#endif

#endif
