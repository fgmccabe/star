//
// Created by Francis McCabe on 1/12/24.
//

#ifndef STAR_VECT_H
#define STAR_VECT_H

// Interface to the standard vector type

#include "normal.h"

termPo vectElement(normalPo v,integer ix);
normalPo buildVector(integer count, ptrPo els);
integer vectorLength(normalPo v);
integer vectorDepth(normalPo v);

#endif //STAR_VECT_H
