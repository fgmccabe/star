#ifndef _LIST_P_H_
#define _LIST_P_H_
/*
  Cons Lists
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
#include "cons.h"
#include "objectP.h"

typedef struct {
  objectPo head;
  consPo tail;
} ConsObjectRec;

typedef struct _cons_record_ {
  ObjectRec object;                     /* object level of the list structure */
  ConsObjectRec list;                   // List part of object
} ConsRecord;

typedef struct {

} ConsClassPart;

typedef struct _cons_class_ {
  ObjectClassRec objectPart;
  ConsClassPart consPart;
} ConsClassRec;

extern ConsClassRec ConsClass;

#endif
