//
// Created by Francis McCabe on 6/18/17.
//

#ifndef CAFE_TERM_H
#define CAFE_TERM_H

#include "config.h"
#include "heap.h"

typedef struct special_class *specialClassPo;

typedef struct enum_struct *termClassPo;

extern termPo allocateEnum(heapPo H,char *nm,int64 arity);

extern termPo allocateObject(heapPo H,termPo cons);

extern termPo nthArg(termPo term,int64 nth);

extern void setArg(termPo term,int64 ix, termPo arg);

#endif //CAFE_TERM_H
