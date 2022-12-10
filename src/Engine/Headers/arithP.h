//
// Created by Francis McCabe on 1/23/18.
//

#ifndef STAR_ARITHP_H
#define STAR_ARITHP_H

#include "heap.h"
#include "arith.h"
#include "code.h"
#include "termP.h"

typedef struct float_term {
  clssPo clss;                  // == floatClass
  double dx;
} FloatRecord;

#define FloatCellCount CellCount(sizeof(FloatRecord))

extern termPo allocateFloat(heapPo H, double dx);

#define EPSILON ((double)1.0e-20)

#endif //STAR_ARITHP_H
