/*
   Managed object class -- the class keeps track of all created instances. 
   Private header file
  Copyright (c) 2016, 2017. Francis G. McCabe

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file
  except in compliance with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software distributed under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
  KIND, either express or implied. See the License for the specific language governing
  permissions and limitations under the License.
*/
#ifndef _MANAGED_P_LIB_H_
#define _MANAGED_P_LIB_H_

#include "retcode.h"
#include "logical.h"
#include "managed.h"
#include "lockableP.h"

/* Class definition types */
typedef struct {
  managedPo instances;                  /* All the instances of the class */
} ManagedClassPartRec;

typedef struct _managed_class_ {
  ObjectClassRec objectPart;
  LockClassPart lockClassPart;
  ManagedClassPartRec managedPart;
} ManagedClassRec;

/* Instance definition types */

typedef struct _managed_part {
  managedPo prev;
  /* previous in the chain */
  managedPo next;
} ManagedRec;

typedef struct _managed_object_ {
  ObjectRec object;                     // The object part of the managed object
  LockObjectRec lock;
  ManagedRec managed;                   // The managed part of the managed object
} ManagedObject;

extern ManagedClassRec ManagedClass;
/* The managed class definition */
extern classPo managedClass;    /* The pointer to the managed class */

#endif
  
