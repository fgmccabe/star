//
// Created by Francis McCabe on 11/10/21.
//

#ifndef STAR_BIGNUM_H
#define STAR_BIGNUM_H

#include "term.h"

#include "multiP.h"

// A bignum is stored as a sequence of 32bit 'digits'

typedef struct bignum_term_ *bignumPo;

extern clssPo bignumClass;

logical isBignum(termPo t);

extern bignumPo C_BIGNUM(termPo t);

uint32 *bigDigits(bignumPo bg);
uint32 bigCount(bignumPo b);
integer bignumHash(bignumPo bg);

#endif //STAR_BIGNUM_H
