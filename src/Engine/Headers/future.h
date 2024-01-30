//
// Created by Francis McCabe on 1/7/24.
//

#ifndef STAR_FUTURE_H
#define STAR_FUTURE_H

#include "term.h"
#include "heap.h"

typedef struct future_record *futurePo;

extern clssPo futureClass;

extern futurePo C_FUTURE(termPo t);

typedef retCode (*futurePoll)(futurePo ft, heapPo h, void *cl, void *cl2);

futurePo makeFuture(heapPo H, termPo vl, futurePoll poll, void *cl, void *cl2);

logical futureIsResolved(futurePo t, heapPo h);
logical futureIsAccepted(futurePo t);
logical futureIsRejected(futurePo t);

retCode resolveFuture(futurePo p, termPo vl);
retCode rejectFuture(futurePo p, termPo ex);

termPo futureValue(futurePo p);

#endif //STAR_FUTURE_H
