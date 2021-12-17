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


uint32 *multiData(multiPo num);
integer multiSize(multiPo num);
sign multiSign(multiPo num);

logical multiNegative(multiPo num);
multiPo multiPlus(multiPo lhs, multiPo rhs);
multiPo multiMinus(multiPo lhs, multiPo rhs);
multiPo multiTimes(multiPo lhs, multiPo rhs);
retCode multiDivide(multiPo *quot, multiPo *rem, multiPo lhs, multiPo rhs);
comparison multiCompare(multiPo a, multiPo b);
logical sameMulti(multiPo a, multiPo b);
multiPo multiGCD(multiPo a, multiPo b);

multiPo allocMulti(uint32 *data, integer count);
void freeMulti(multiPo m);

multiPo multiFromStr(char *str);
multiPo multiFromText(char *text, integer tlen);

integer formatMulti(multiPo num, const char *format, integer formatLen, char *buffer, integer buffLen);
retCode showMulti(ioPo out, void *data, long depth, long precision, logical alt);

#endif //STAR_MULTI_H
