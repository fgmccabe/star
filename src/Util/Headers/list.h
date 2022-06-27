#ifndef _LIST_H_
#define _LIST_H_
/*
  Cons Lists
  Copyright (c) 2016, 2017. Francis G. McCabe
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
