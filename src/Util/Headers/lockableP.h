/*
  Lockable objects - private header
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _U_LOCKABLEP_H
#define _U_LOCKABLEP_H

#include "lockable.h"
#include "objectP.h"

typedef struct {
  pthread_mutex_t mutex;		            /* Mutex for the lock */
} LockObjectRec;

typedef struct _lock_record_ {
  ObjectRec object;                     /* object level of the structure */
  LockObjectRec lock;                   // Lock part of object
} LockedRecord;

typedef struct {

} LockClassPart;

typedef struct _list_class_ {
  ObjectClassRec objectClassPart;
  LockClassPart lockClassPart;
} LockClassRec;

extern LockClassRec LockedClass;

#endif //_U_LOCKABLEP_H
