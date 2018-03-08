//
// Created by Francis McCabe on 3/4/18.
//

#ifndef CAFE_ARRAY_H
#define CAFE_ARRAY_H

#include "term.h"

typedef struct _list_slice_ *listPo;

extern clssPo listClass;

extern listPo C_LIST(termPo t);

typedef retCode (*listProc)(termPo el, integer ix, void *cl);

extern retCode processList(listPo list, listProc p, logical safeMode, void *cl);

extern integer listSize(listPo list);
extern termPo nthEl(listPo list, integer ix);



#endif //CAFE_ARRAY_H
