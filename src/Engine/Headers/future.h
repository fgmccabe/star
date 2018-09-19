//
// Created by Francis McCabe on 6/7/18.
//

#ifndef STAR_FUTURE_H
#define STAR_FUTURE_H

#include "term.h"

typedef struct _future_record_ *futurePo;

extern clssPo futureClass;

extern futurePo C_FUTURE(termPo t);

extern logical futureHasValue(futurePo ft);

extern termPo getFutureGenerator(futurePo ft);

extern termPo getFutureValue(futurePo ft);

extern termPo setFutureValue(futurePo ft,termPo e);

#endif //STAR_FUTURE_H
