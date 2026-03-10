//
// Created by Francis McCabe on 3/6/26.
//

#ifndef STAR_DISASS_H
#define STAR_DISASS_H

#include "code.h"
#include "ooio.h"
#include "stack.h"

ssaInsPo disass(ioPo out, stackPo stk, methodPo mtd, ssaInsPo pc);

#endif //STAR_DISASS_H
