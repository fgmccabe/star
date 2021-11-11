//
// Created by Francis McCabe on 11/10/21.
//

#ifndef STAR_BCD_H
#define STAR_BCD_H

#include "term.h"

// A bcd structure can store an arbitrarily large decimal number
// Stored as a sequence of decimal nibbles

typedef struct bcd_term_ *bcdPo;

extern clssPo bcdClass;

logical isBcd(termPo t);

integer bcdLen(bcdPo b);
byte *bcdData(bcdPo b);

#endif //STAR_BCD_H
