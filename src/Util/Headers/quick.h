//
// Created by Francis McCabe on 9/17/23.
//

#ifndef STAR_QUICK_H
#define STAR_QUICK_H

#include "config.h"
#include "retcode.h"
#include "integer.h"

typedef comparison (*compare)(integer el1,integer el2, void *cl);
typedef retCode (*swap)(integer el1,integer el2, void *cl);

retCode quick(integer from, integer to, compare cmp, swap swp, void *cl);

#endif //STAR_QUICK_H
