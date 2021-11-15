//
// Created by Francis McCabe on 11/12/21.
//

#ifndef STAR_BCD_H
#define STAR_BCD_H

#include "config.h"
#include "ooio.h"

typedef enum {
  positive,
  negative
} sign;

typedef struct bcd_number_ *bcdPo;

sign bcdSign(bcdPo num);
bcdPo bcdPlus(bcdPo lhs, bcdPo rhs);
bcdPo bcdMinus(bcdPo lhs, bcdPo rhs);
integer bcdTimes(bcdPo lhs, bcdPo rhs, bcdPo res);
integer bcdDivide(bcdPo lhs, bcdPo rhs, bcdPo res);
integer bcdQuotient(bcdPo lhs, bcdPo rhs, bcdPo res);
integer bcdRemainder(bcdPo lhs, bcdPo rhs, bcdPo res);

logical sameBCD(bcdPo lhs, bcdPo rhs);

integer textOfBCD(char *text, integer tlen, bcdPo num);
bcdPo bcdFromText(char *text, integer tlen);

#endif // STAR_BCD_H
