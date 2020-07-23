#ifndef _LIST_H_
#define _LIST_H_
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


#include "object.h"
#include "iterate.h"

typedef struct list_record_ *listPo;

extern void *head(listPo list);
extern listPo tail(listPo list);

extern listPo cons(objectPo head, listPo tail);
extern listPo tack(objectPo head, listPo list);

extern objectPo listNthElement(listPo list, int64 ix);

extern long listCount(listPo list);

extern listPo nilList;

typedef retCode (*listFun)(objectPo data,void *cl);

extern retCode processCons(listPo list, listFun fun, void *cl);

typedef logical (*listTest)(objectPo data,void *cl);
extern void* findInList(listPo list, listTest test, void *cl);

extern listPo removeElements(listPo l, listTest test, void *cl);
extern listPo filter(listPo l, listTest test, void *cl);

typedef void *(*folder)(objectPo value, void *state);

extern void* listFold(listPo l, folder f, void *state);

extern listPo sortCons(listPo l, objCompare compare, void *cl);

extern void releaseList(listPo l);

extern classPo listClass;

#ifdef VERIFY_OBJECT
#define O_LIST(c) ((listPo)(checkCast((c),listClass)))
#else
#define O_LIST(c) ((consPo)(c))
#endif

#endif
