//
// Created by Francis McCabe on 1/14/20.
//

#ifndef STAR_MEMO_H
#define STAR_MEMO_H

#include "term.h"
#include "heap.h"

typedef struct _memo_rec_ *memoPo;

extern clssPo memoClass;

extern memoPo C_MEMO(termPo t);

extern memoPo memoVar(heapPo H,termPo provider,termPo content);

extern logical memoIsSet(memoPo memo);

extern termPo memoGetContent(memoPo memo);
extern termPo memoGetProvider(memoPo memo);
extern retCode memoSetValue(memoPo memo, termPo value);

#endif //STAR_MEMO_H
