//
// Created by Francis McCabe on 11/10/21.
//

#ifndef STAR_BIGNUM_H
#define STAR_BIGNUM_H

#include "term.h"

// A bcd structure can store an arbitrarily large decimal number
// Stored as a sequence of decimal nibbles

typedef struct bignum_term_ *bignumPo;

extern clssPo bignumClass;

logical isBignum(termPo t);


#endif //STAR_BIGNUM_H
