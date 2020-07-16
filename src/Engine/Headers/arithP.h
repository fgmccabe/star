//
// Created by Francis McCabe on 1/23/18.
//

#ifndef STAR_ARITHP_H
#define STAR_ARITHP_H

#include "heap.h"
#include "arith.h"
#include "code.h"
#include "termP.h"

typedef struct integer_term *intPo;

extern intPo C_INT(termPo t);

typedef struct integer_term {
  clssPo clss;                  // == integerClass
  integer ix;
} IntegerRecord;

#define IntegerCellCount CellCount(sizeof(IntegerRecord))

extern termPo allocateInteger(heapPo H, integer ix);

typedef struct float_term {
  clssPo clss;                  // == floatClass
  double dx;
} FloatRecord;

#define FloatCellCount CellCount(sizeof(FloatRecord))

extern termPo allocateFloat(heapPo H, double dx);

#define EPSILON ((double)1.0e-20)

#endif //STAR_ARITHP_H
