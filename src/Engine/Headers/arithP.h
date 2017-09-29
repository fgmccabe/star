//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_ARITHP_H_H
#define CAFE_ARITHP_H_H

#include "arith.h"

// Integer structure
typedef struct int_struct {
  Term termPart;          // == integerCLass
  int64 ix;
} IntegerRec;

#define IntegerCellCount CellCount(sizeof(IntegerRec))

// Float structure
typedef struct float_struct {
  Term termPart;          // == floatClass
  double dx;
} FloatRec;

#define FloatCellCount CellCount(sizeof(FloatRec))

#endif //CAFE_ARITHP_H_H
