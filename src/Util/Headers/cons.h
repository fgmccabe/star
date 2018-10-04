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

typedef struct _cons_record_ *consPo;

extern void *head(consPo list);
extern consPo tail(consPo list);

extern consPo cons(objectPo head,consPo tail);
extern consPo tack(objectPo head,consPo list);

extern objectPo listNthElement(consPo list, int64 ix);

extern long listCount(consPo list);

extern consPo nilList;

typedef retCode (*listFun)(objectPo data,void *cl);

extern retCode processCons(consPo list, listFun fun, void *cl);

typedef logical (*listTest)(objectPo data,void *cl);
extern void* findInList(consPo list,listTest test,void *cl);

extern consPo removeElements(consPo l,listTest test,void *cl);
extern consPo filter(consPo l, listTest test, void *cl);

typedef void *(*folder)(objectPo value, void *state);

extern void* listFold(consPo l,folder f,void *state);

extern consPo sortCons(consPo l,objCompare compare,void *cl);

extern void releaseList(consPo l);

extern classPo listClass;

#ifdef VERIFY_OBJECT
#define O_LIST(c) ((consPo)(checkCast((c),listClass)))
#else
#define O_LIST(c) ((consPo)(c))
#endif

#endif
