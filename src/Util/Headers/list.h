#ifndef _LIST_H_
#define _LIST_H_

#include <retcode.h>
#include <logical.h>
#include <iterate.h>

typedef struct _list_record_ *listPo;

extern void *head(listPo list);
extern listPo tail(listPo list);

extern listPo cons(void *head,listPo tail);
extern listPo tack(void *head,listPo list);

extern void* listNthElement(listPo list,int32 ix);

extern long listCount(listPo list);

extern logical sameLists(listPo l1,listPo l2);

extern void releaseList(listPo list);

extern listPo emptyList;

typedef retCode (*listFun)(void *data,void *cl);

extern retCode processList(listPo list,listFun fun,void *cl);

typedef logical (*listTest)(void* data,void *cl);
extern void* findInList(listPo list,listTest test,void *cl);

extern listPo removeElements(listPo l,listTest test,void *cl);

#endif
