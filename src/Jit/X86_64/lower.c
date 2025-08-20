//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "x86_64.h"

/* Lower Star VM code to X64 code */
/*
 *  First four arguments passed in registers:
 *  A0 = RCX
 *  A1 = RDX
 *  A2 = R8
 *  A3 = R9
 *
 *  RBP is the frame pointer
 *  R10 points to the literals tuple
 *
 *  RAX is the return value
 */

registerMap defltAvailRegSet() {
  return (1<<RAX)|(1<<RCX)|(1<RBX)|(1<<RSI)|(1<<RDI)|
    (1<<R8)|(1<<R9)|(1<<R10)|(1<<R11)|(1<<R12)|(1<<R13)|(1<<R14)|(1<<R15);
}


retCode jit_preamble(methodPo mtd, jitCompPo jitCtx) {
  return Error;
}

retCode jit_postamble(methodPo mtd, jitCompPo jitCtx) {
  return Error;
}

static int32 collectOperand(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

x64Op formX64Operand(vOperand v) {
  switch (v.loc) {
    case argument: {
      switch (v.ix) {
        case 0: {
          x64Op op = {.mode=Reg, .op.reg=RCX};
          return op;
        }
        case 1: {
          x64Op op = {.mode=Reg, .op.reg=RDX};
          return op;
        }
        case 2: {
          x64Op op = {.mode=Reg, .op.reg=R8};
          return op;
        }
        case 3: {
          x64Op op = {.mode=Reg, .op.reg=R9};
          return op;
        }
        default: {
          x64Op op = {.mode=Based, .op.based.base=RBP, .op.based.disp=(int) (v.ix * LONG_COUNT + FRAME_SIZE)};
          return op;
        }
      }
    }
    case literal: {
      x64Op op = {.mode=Based, .op.based.base=R10, .op.based.disp=(int) (v.ix * LONG_COUNT)};
      return op;
    }
    case local: {
      x64Op op = {.mode=Based, .op.based.base=RBP, .op.based.disp=(int) (-v.ix * LONG_COUNT)};
      return op;
    }
    case mcReg: {
      return v.mcLoc;
    }
  }
}

static x64Op popStkOp(jitCompPo jitCtx) {
  verifyJitCtx(jitCtx, 1, 0);
  vOperand v = jitCtx->vStack[--jitCtx->vTop];
  return formX64Operand(v);
}

static void pushStkOp(jitCompPo jitCtx, x64Op operand) {
  verifyJitCtx(jitCtx, 0, 1);
  vOperand v = {.loc=mcReg, .mcLoc=operand};
  jitCtx->vStack[jitCtx->vTop++] = v;
}
