//
// Created by Francis McCabe on 1/7/24.
//

#ifndef STAR_FUTURE_H
#define STAR_FUTURE_H

#include "term.h"
#include "heap.h"

typedef struct future_record* futurePo;

extern futurePo C_FUTURE(termPo t);

typedef retCode (*futurePoll)(futurePo ft, void* cl, void* cl2);

futurePo makeFuture(termPo vl, futurePoll poll, void* cl, void* cl2);

logical futureIsResolved(futurePo t);
logical futureIsAccepted(futurePo t);
logical futureIsRejected(futurePo t);

retCode resolveFuture(futurePo p, termPo vl);
retCode rejectFuture(futurePo p, termPo ex);

termPo futureValue(futurePo p);

#endif //STAR_FUTURE_H
