//
// Created by Francis McCabe on 3/4/18.
//

#ifndef CAFE_ARRAY_H
#define CAFE_ARRAY_H

#include "term.h"
#include "heap.h"

typedef struct _list_slice_ *listPo;

extern clssPo listClass;

extern listPo C_LIST(termPo t);

extern listPo allocateList(heapPo H, integer length, logical safeMode);

extern listPo createList(heapPo H, integer capacity);

extern termPo sliceList(heapPo H, listPo list, integer from, integer count);

extern listPo appendToList(heapPo H, listPo list, termPo el);

extern listPo concatList(heapPo H, listPo l1,listPo l2);

extern listPo flattenList(heapPo H, listPo l);

extern listPo reverseList(heapPo H, listPo l1);

// Use with caution!
extern listPo addToList(heapPo H, listPo list, termPo el);

extern termPo prependToList(heapPo H, listPo list, termPo el);

typedef retCode (*listProc)(termPo el, integer ix, void *cl);

extern retCode processList(listPo list, listProc p, void *cl);

extern integer listSize(listPo list);
extern termPo nthEl(listPo list, integer ix);
extern void setNthEl(listPo list,integer ix,termPo el);



#endif //CAFE_ARRAY_H
