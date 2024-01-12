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

futurePo makeFuture(heapPo H, termPo vl, termPo queue);

logical futureIsResolved(futurePo t);
logical futureIsAccepted(futurePo t);
logical futureIsRejected(futurePo t);

retCode resolveFuture(futurePo p, termPo vl);
retCode rejectFuture(futurePo p, termPo ex);

termPo futureValue(futurePo p);
termPo futureQueue(futurePo p);

#endif //STAR_FUTURE_H
