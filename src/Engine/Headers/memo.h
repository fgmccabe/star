//
// Created by Francis McCabe on 1/14/20.
//

#ifndef STAR_MEMO_H
#define STAR_MEMO_H

#include "term.h"
#include "heap.h"

typedef struct memo_rec_ *memoPo;

extern clssPo memoClass;

extern memoPo C_MEMO(termPo t);

extern memoPo memoVar(heapPo H, normalPo provider);

extern logical isMemoSet(memoPo memo);

extern termPo getMemoContent(memoPo memo);
extern normalPo getMemoProvider(memoPo memo);
extern retCode setMemoValue(memoPo memo, termPo value);

#endif //STAR_MEMO_H
