
//
// Created by Francis McCabe on 10/2/21.
//

#ifndef STAR_THUNK_H
#define STAR_THUNK_H

#include "term.h"
#include "heap.h"
#include "closure.h"

typedef struct thunk_rec_ *thunkPo;

extern clssPo thunkClass;

thunkPo C_THUNK(termPo t);

thunkPo thunkVar(heapPo H, closurePo lam);
termPo thunkVal(thunkPo v);
termPo setThunk(thunkPo v, termPo e);
logical thunkIsSet(thunkPo thnk);
closurePo thunkLam(thunkPo thunk);

#endif //STAR_THUNK_H
