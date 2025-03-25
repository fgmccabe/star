//
// Created by Francis McCabe on 3/21/25.
//

#ifndef STAR_LIFOP_H
#define STAR_LIFOP_H

#include "lifo.h"

typedef struct lifo_element_ {
  void *el;
  lifoPo prev;
} LifoElementRec;

void initLifo();

#endif //STAR_LIFOP_H
