//
// Created by Francis McCabe on 9/10/21.
//

#ifndef STAR_FUTURE_H
#define STAR_FUTURE_H

#include "stack.h"
typedef struct future_record_ *futurePo;

extern clssPo futureClass;

extern futurePo C_FUTURE(termPo t);

typedef void (*futureSetProc)(futurePo f, termPo cl);
futurePo makeFuture(heapPo H, futureSetProc fut);

logical futureIsSet(futurePo t);

retCode setFuture(heapPo H, futurePo ft, termPo val);
termPo getFuture(futurePo f);
#endif //STAR_FUTURE_H
