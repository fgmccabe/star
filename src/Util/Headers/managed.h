/*
 * Managed object class -- the class keeps track of all created instances. 
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/ 
#ifndef _MANAGED_LIB_H_
#define _MANAGED_LIB_H_

#include "lockable.h"

typedef struct _managed_object_ *managedPo;
extern classPo managedClass;

// A function type to apply to all managed instances
typedef retCode (*manageProc)(managedPo o,void *cd);

retCode processAll(classPo class,manageProc proc,void *cd);
void destroyAll(classPo class);

#ifdef VERIFY_OBJECT
#define O_MANAGED(c) ((managedPo)(checkCast((c),managedClass)))
#else
#define O_MANAGED(c) ((managedPo)(c))
#endif

#endif
