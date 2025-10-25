//
// Created by Francis McCabe on 10/24/25.
//

#include <config.h>

#include "sort.h"
#include "lowerP.h"
#include "shuffle.h"

static logical usesReg(FlexOp op, mcRegister Rg) {
  switch (op.mode) {
    case reg:
    case sOff:
      return op.reg == Rg;
    case preX:
      return op.reg == Rg || op.rgm == Rg;
    default:
      return False;
  }
}

logical affects(FlexOp src, FlexOp dst) {
  switch (src.mode) {
    case reg:
      return usesReg(dst, src.reg);
    case sOff: {
      switch (dst.mode) {
        case reg:
          return dst.reg == src.reg;
        case sOff:
          return dst.reg == src.reg && dst.immediate == src.immediate;
        default:
          return False;
      }
    }
    default:
      return False;
  }
}

static void collectGroup(argSpecPo args, int32 arity, int32 groupNo, argSpecPo *group) {
  int32 pt = 0;
  for (int32 ix = 0; ix < arity; ix++) {
    if (args[ix].group == groupNo) {
      group[pt++] = &args[ix];
    }
  }
}

void shuffleVars(assemCtxPo ctx,
                 argSpecPo args,
                 int32 arity,
                 registerMap *freeRegs,
                 moveFunc mover) {
  int32 groups = sortSpecs(args, arity);

  for (int32 gx = groups; gx > 0; gx--) {
    int32 grpSize = groupSize(args, arity, gx - 1);
    argSpecPo group[grpSize];

    collectGroup(args, arity, gx - 1, group);

    if (grpSize == 1) {
      group[0]->mark = True;
      FlexOp dst = group[0]->dst;

      if (!sameFlexOp(dst, group[0]->src)) {
        mover(ctx, group[0]->dst, group[0]->src, freeRegs);
      }
      group[0]->group = -1;
    } else {
      mcRegister tmp = nxtAvailReg(*freeRegs);
      check(tmp!=XZR, "no available registers");
      *freeRegs = dropReg(*freeRegs, tmp);

      FlexOp dst = group[0]->dst;
      FlexOp src = group[0]->src;
      int32 grpIx = 0;

      mover(ctx,RG(tmp), dst, freeRegs);
      do {
        for (int32 ix = 0; ix < grpSize; ix++) {
          if (sameFlexOp(src, group[ix]->dst)) {
            mover(ctx, group[ix]->dst, RG(tmp), freeRegs);
            mover(ctx, RG(tmp), group[ix]->src, freeRegs);
            src = group[ix]->src;
            grpIx = ix;
            break;
          }
        }
      } while (!sameFlexOp(src, group[0]->src));

      mover(ctx, dst,RG(tmp), freeRegs);

      *freeRegs = addReg(*freeRegs, tmp);
    }
  }
}
