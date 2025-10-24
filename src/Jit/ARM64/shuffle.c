//
// Created by Francis McCabe on 10/24/25.
//

#include <config.h>

#include "sort.h"
#include "lowerP.h"
#include "shuffle.h"

static logical usesReg(FlexOp op, mcRegister Rg)
{
  switch (op.mode){
  case reg:
  case sOff:
    return op.reg == Rg;
  case preX:
    return op.reg == Rg || op.rgm == Rg;
  default:
    return False;
  }
}

logical affects(FlexOp src, FlexOp dst)
{
  switch (src.mode){
  case reg:
    return usesReg(dst, src.reg);
  case sOff:
    {
      switch (dst.mode){
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

static logical clobbers(argSpecPo def, argSpecPo ref)
{
  return affects(def->dst, ref->src);
}

static void collectGroup(argSpecPo args, int32 arity, int32 groupNo, argSpecPo* group)
{
  int32 pt = 0;
  for (int32 ix = 0; ix < arity; ix++){
    if (args[ix].group == groupNo){
      group[pt++] = &args[ix];
    }
  }
}

static logical marked(argSpecPo args, int32 arity, logical mark)
{
  for (int32 ix = 0; ix < arity; ix++){
    if (args[ix].mark != mark)
      return False;
  }
  return True;
}

static logical isClobbered(argSpecPo args, int32 arity, argSpecPo ref)
{
  for (int32 ix = 0; ix < arity; ix++){
    if (args[ix].mark && clobbers(&args[ix], ref))
      return True;
  }
  return False;
}

void shuffleVars(assemCtxPo ctx,
                 argSpecPo args,
                 int32 arity,
                 registerMap* freeRegs,
                 moveFunc mover)
{
  int32 groups = sortSpecs(args, arity);

  assert(marked(args,arity,False));

  for (int32 gx = groups; gx > 0; gx--){
    int32 grpSize = groupSize(args, arity, gx - 1);
    argSpecPo group[grpSize];

    collectGroup(args, arity, gx - 1, group);

    if (grpSize == 1){
      assert(!group[0]->mark);
      assert(!isClobbered(args,arity,group[0]));
      group[0]->mark = True;
      FlexOp dst = group[0]->dst;

      if (!sameFlexOp(dst, group[0]->src)){
        mover(ctx, group[0]->dst, group[0]->src, freeRegs);
      }
      group[0]->group = -1;
    }
    else{
      mcRegister tmp = nxtAvailReg(*freeRegs);
      check(tmp!=XZR, "no available registers");
      *freeRegs = dropReg(*freeRegs, tmp);

      FlexOp dst = group[0]->dst;

      mover(ctx,RG(tmp), dst, freeRegs);
      for (int32 ix = 0; ix < grpSize - 1; ix++){
        mover(ctx, group[ix]->dst, group[ix + 1]->dst, freeRegs);
        assert(!group[ix]->mark);
        assert(!isClobbered(args,arity,group[ix]));
        group[ix]->mark = True;
      }

      mover(ctx, group[grpSize - 1]->dst,RG(tmp), freeRegs);

      *freeRegs = addReg(*freeRegs, tmp);
    }
  }
  assert(marked(args,arity,True));
}
