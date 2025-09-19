//
// Created by Francis McCabe on 3/21/25.
//

#ifndef STAR_LIFO_H
#define STAR_LIFO_H

#include "config.h"
#include "integer.h"

typedef struct lifo_element_ *lifoPo;

lifoPo pushElement(void *el,lifoPo stk);
lifoPo popElement(void **top, lifoPo stk);
void *peekElement(lifoPo stk, int32 ix);
lifoPo dropElement(lifoPo stk, int32 ix);

integer lifoCount(lifoPo stk);

#endif //STAR_LIFO_H
