/*
  Lockable objects
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _U_LOCKABLE_H
#define _U_LOCKABLE_H

#include "object.h"
#include "config.h"

typedef struct _lock_record_ *lockedPo;

void lock(lockedPo o);
retCode timedLock(lockedPo l, double tmout);
void unlock(lockedPo o);

extern classPo lockedClass;

#ifdef VERIFY_OBJECT
#define O_LOCKED(c) ((lockedPo)(checkCast((c),lockedClass)))
#else
#define O_LOCKED(c) ((lockedPo)(c))
#endif


#endif //_U_LOCKABLE_H
