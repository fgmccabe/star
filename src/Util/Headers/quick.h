//
// Created by Francis McCabe on 9/17/23.
//

#ifndef STAR_QUICK_H
#define STAR_QUICK_H

#include "config.h"
#include "retcode.h"
#include "integer.h"

typedef comparison (*compare)(int32 el1,int32 el2, void *cl);
typedef retCode (*swap)(int32 el1,int32 el2, void *cl);

retCode quick(int32 from, int32 to, compare cmp, swap swp, void *cl);

#endif //STAR_QUICK_H
