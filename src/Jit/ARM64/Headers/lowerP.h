//
// Created by Francis McCabe on 10/11/21.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "hash.h"
#include "lower.h"
#include "jitP.h"
#include "arm64P.h"
#include "macros.h"

/* Lower Star VM code to Arm64 code */
/* Register allocation for arm64:
 *
 * SP = X31 = stack pointer
 * FP = X29 = frame pointer
 * LR = X30 = link register
 * X0-X8 = integer parameters
 * X0 = return register
 * X9-X15 = caller saved scratch registers
 * X16-X17 = intra procedure call scratch registers
 * X18 = platform register
 * X19-X26 = callee saved registers
 * SB = X27 = current stack structure pointer
 * PL = X28 = constant pool pointer
 */

#define SB (X27)
#define PL (X28)


#endif //STAR_LOWERP_H
