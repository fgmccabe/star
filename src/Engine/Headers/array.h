//
// Created by Francis McCabe on 3/4/18.
//

#ifndef STAR_ARRAY_H
#define STAR_ARRAY_H

#include "term.h"
#include "heap.h"

typedef struct _list_slice_ *listPo;

extern clssPo listClass;

extern listPo C_LIST(termPo t);

typedef enum{
  safeAlloc,
  fastAlloc
} allocSafety;

extern listPo allocateList(heapPo H, integer length);

extern listPo createList(heapPo H, integer capacity);

extern termPo sliceList(heapPo H, listPo list, integer from, integer count);

extern listPo spliceList(heapPo H, listPo list, integer from, integer count, listPo rep);

extern listPo appendToList(heapPo H, listPo list, termPo el);

extern listPo concatList(heapPo H, listPo l1, listPo l2);

extern listPo flattenList(heapPo H, listPo l);

extern listPo reverseList(heapPo H, listPo l1);

extern listPo insertListEl(heapPo H, listPo list, integer px, termPo vl);

extern listPo replaceListEl(heapPo H, listPo list, integer px, termPo vl);

extern listPo removeListEl(heapPo H, listPo list, integer px);

// Use with caution!

extern listPo prependToList(heapPo H, listPo list, termPo el);

typedef retCode (*listProc)(termPo el, integer ix, void *cl);

extern retCode processList(listPo list, listProc p, void *cl);

extern integer listSize(listPo list);
extern termPo nthEl(listPo list, integer ix);
extern void setNthEl(listPo list, integer ix, termPo el);

#endif //STAR_ARRAY_H
