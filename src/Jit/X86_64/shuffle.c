//
// Created by Francis McCabe on 10/24/25.
//

#include <config.h>

#include "sort.h"
#include "lowerP.h"
#include "shuffle.h"

static logical usesReg(FlexOp op, mcRegister Rg) {
  switch (op.mode) {
  case Reg:
    return op.op.reg == Rg;
  case Based:
    return op.op.based.base == Rg;
  case Indexed:
    return op.op.indexed.base == Rg || op.op.indexed.index == Rg;
  default:
    return False;
  }
}

logical affects(FlexOp src, FlexOp dst) {
  switch (src.mode) {
  case Reg:
    return usesReg(dst, src.op.reg);
  case Based: {
    switch (dst.mode) {
    case Reg:
      return dst.op.reg == src.op.based.base;
    case Based:
      return dst.op.based.base == src.op.based.base && dst.op.based.disp == src.op.based.disp;
    default:
      return False;
    }
  }
  default:
    return False;
  }
}

static void collectGroup(argSpecPo args, int32 arity, int32 groupNo, argSpecPo* group) {
  int32 pt = 0;
  for (int32 ix = 0; ix < arity; ix++) {
    if (args[ix].group == groupNo) {
      group[pt++] = &args[ix];
    }
  }
}

static void argMove(jitCompPo jit, FlexOp dst, FlexOp src, registerMap* freeRegs) {
  move(assemCtx(jit), dst, src, *freeRegs);
}

void shuffleVars(jitCompPo jit, argSpecPo args, int32 arity, registerMap* freeRegs) {
  assemCtxPo ctx = assemCtx(jit);
  int32 groups = sortSpecs(args, arity);

  for (int32 gx = groups; gx > 0; gx--) {
    int32 grpSize = groupSize(args, arity, gx - 1);
    argSpecPo group[grpSize];

    collectGroup(args, arity, gx - 1, group);

    if (grpSize == 1) {
      group[0]->mark = True;
      FlexOp dst = group[0]->dst;

      if (!sameFlexOp(dst, group[0]->src)) {
        argMove(jit, group[0]->dst, group[0]->src, freeRegs);
      }
      group[0]->group = -1;
    }
    else {
      mcRegister tmp = nxtAvailReg(*freeRegs);
      check(tmp!=XZR, "no available registers");
      *freeRegs = dropReg(*freeRegs, tmp);

      FlexOp dst = group[0]->dst;
      FlexOp src = group[0]->src;

      move(ctx, RG(tmp), dst, *freeRegs);
      argMove(jit, dst, src, freeRegs);
      do {
        for (int32 ix = 0; ix < grpSize; ix++) {
          if (sameFlexOp(src, group[ix]->dst)) {
            if (sameFlexOp(group[ix]->src, dst))
              argMove(jit, group[ix]->dst, RG(tmp), freeRegs);
            else
              argMove(jit, group[ix]->dst, group[ix]->src, freeRegs);
            src = group[ix]->src;
            break;
          }
        }
      }
      while (!sameFlexOp(src, dst));
      *freeRegs = addReg(*freeRegs, tmp);
    }
  }
}
