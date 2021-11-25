//
// Created by Francis McCabe on 11/23/21.
//

#ifndef STAR_MULTI_H
#define STAR_MULTI_H

#include "config.h"
#include "integer.h"
#include "io.h"

// Multi-precision arithmetic
typedef struct multi_record_ *multiPo;

byte *multiData(multiPo num);
integer multiSize(multiPo num);

logical multiNegative(multiPo num);
multiPo multiPlus(multiPo lhs, multiPo rhs);
multiPo multiMinus(multiPo lhs, multiPo rhs);
multiPo multiTimes(multiPo lhs, multiPo rhs);
integer multiDivide(multiPo lhs, multiPo rhs, multiPo res);
integer multiQuotient(multiPo lhs, multiPo rhs, multiPo res);
integer multiRemainder(multiPo lhs, multiPo rhs, multiPo res);
comparison multiCompare(multiPo a, multiPo b);

multiPo allocMulti(byte *data, integer count);
integer textOfmulti(char *text, integer tlen, multiPo num);
multiPo multiFromText(char *text, integer tlen);

retCode showMulti(ioPo out, void *data, long depth, long precision, logical alt);

// Testing only
integer multiText(char *text, integer tLen, multiPo num);

#endif //STAR_MULTI_H
