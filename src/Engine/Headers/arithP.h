//
// Created by Francis McCabe on 1/23/18.
//

#ifndef CAFE_ARITHP_H
#define CAFE_ARITHP_H

#include "heap.h"
#include "arith.h"
#include "code.h"
#include "termP.h"

typedef struct integer_term {
  clssPo clss;                  // == integerClass
  integer ix;
} IntegerRecord;

#define IntegerCellCount CellCount(sizeof(IntegerRecord))

extern intPo allocateInteger(heapPo H, integer ix);

typedef struct float_term {
  clssPo clss;                  // == floatClass
  double dx;
} FloatRecord;

#define FloatCellCount CellCount(sizeof(FloatRecord))

extern fltPo allocateFloat(heapPo H, double dx);

#endif //CAFE_ARITHP_H
