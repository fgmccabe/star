//
// Created by Francis McCabe on 10/11/21.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "hash.h"
#include "lower.h"
#include "arm64P.h"
#include "macros.h"

/* Register allocation for arm64:
 *
 * SP = X31 = system stack pointer
 * FP = X29 = frame pointer
 * LR = X30 = link register
 * X0-X8 = integer parameters
 * X0 = return register
 * X9-X15 = caller saved scratch registers
 * X16-X17 = intra procedure call scratch registers
 * X18 = platform register
 * X19-X25 = callee saved registers
 * SB = X27 = current stack structure pointer
 * PL = X28 = constant pool pointer
 */

#endif //STAR_LOWERP_H
