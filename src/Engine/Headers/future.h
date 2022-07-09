//
// Created by Francis McCabe on 9/10/21.
//

#ifndef STAR_FUTURE_H
#define STAR_FUTURE_H

#include "stack.h"
typedef struct future_record_ *futurePo;

extern clssPo futureClass;

extern futurePo C_FUTURE(termPo t);

typedef void (*futureSetProc)(futurePo f);
futurePo makeFuture(heapPo H, futureSetProc fut, void *cl);

retCode attachToFuture(heapPo H, futurePo f, stackPo cont);
logical futureIsSet(futurePo t);

retCode setFuture(heapPo H, futurePo ft, termPo val);

#endif //STAR_FUTURE_H
