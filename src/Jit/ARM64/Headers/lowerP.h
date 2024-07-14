//
// Created by Francis McCabe on 10/11/21.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "hash.h"
#include "lower.h"
#include "jitP.h"
#include "arm64P.h"
/*

 X0-X7 Arguments // Need to be spilled if an allocation in function
 X9-X15 Caller saved
 X19-X29 Callee saved
 X8 indirect result
 X16, X17   IP0, IP1 intra call temps
 X18 platform register
 SB = X27 Base of current stack
 CL = X28 Code literals
 FP = X29 Frame pointer
 LR = X30 Link register
 SP = X31 Stack pointer
 */

// Code literal vector
#define CL (X28)
// Stack base pointer
#define SB (X27)

static inline registerMap callerSaved() {
  return 1u << X9 | 1u << X10 | 1u << X11 | 1u << X12 | 1u << X13 | 1u << X14 | 1u << X15;
}

static inline registerMap calleeSaved() {
  return 1u << X19 | 1u << X20 | 1u << X21 | 1u << X22 | 1u << X23 | 1u << X24 | 1u << X25 | 1u << X26 | 1u << X27 |
         1u << X28 | 1u << X29;
}

static inline registerMap stackRegs() {
  return 1u << X9 | 1u << X10 | 1u << X11 | 1u << X12;
}

registerMap allocReg(registerMap from, armReg Rg);
registerMap freeReg(registerMap from, armReg Rg);

armReg nxtAvailReg(registerMap from);

#endif //STAR_LOWERP_H
