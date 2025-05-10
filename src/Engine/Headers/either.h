//
// Created by Francis McCabe on 3/3/24.
//

#ifndef STAR_EITHER_H
#define STAR_EITHER_H

#include "term.h"
#include "heap.h"
#include "labelsP.h"

logical isNeither(termPo t);
logical isEither(termPo t);
logical isOr(termPo t);

termPo eitherValue(termPo t);
termPo orValue(termPo t);

#endif //STAR_EITHER_H
