//
// Created by Francis McCabe on 9/25/25.
//

#include "shuffle.h"

typedef struct {
  int32 top;
  argSpecPo *stack;
} Stack, *stkPo;

static int32 stackCount(stkPo stack) {
  return stack->top;
}

static argSpecPo stackPeek(stkPo stack, int32 ix) {
  if (ix >= stack->top)
    return Null;
  return stack->stack[stack->top - ix - 1];
}

static argSpecPo stackPop(stkPo stack) {
  if (stack->top == 0)
    return Null;
  return stack->stack[--stack->top];
}

static void stackPush(argSpecPo spec, stkPo stack) {
  stack->stack[stack->top++] = spec;
}

static argSpecPo nextDef(ArgSpec defs[], int32 arity) {
  for (int32 ix = 0; ix < arity; ix++) {
    argSpecPo def = &defs[ix];
    if (def->mark) {
      return def;
    }
  }
  return Null;
}

static logical usesReg(FlexOp op, armReg Rg) {
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

static logical affects(FlexOp src, FlexOp dst) {
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

static logical clobbers(argSpecPo def, argSpecPo ref) {
  return affects(def->dst, ref->src);
}

static int32 analyseDef(argSpecPo def, ArgSpec defs[], int32 arity, stkPo stack, int32 *groups);

static int32 analyseRef(argSpecPo ref, ArgSpec defs[], int32 arity, stkPo stack, int32 *groups, int32 low) {
  // Is this reference already in the stack?
  for (int32 ix = 0; ix < stackCount(stack); ix++) {
    if (ref == stackPeek(stack, ix)) {
      // reference already in stack
      ref->mark = False;
      return min(low, ix);
    }
  }
  // look in definitions
  return analyseDef(ref, defs, arity, stack, groups);
}

argSpecPo findRef(argSpecPo def, ArgSpec defs[], int32 arity, int32 from) {
  for (int32 ix = from; ix < arity; ix++) {
    argSpecPo ref = &defs[ix];
    if (ref->group == -1 && clobbers(def, ref)) {
      return &defs[ix];
    }
  }

  return Null;
}

int32 analyseDef(argSpecPo def, ArgSpec defs[], int32 arity, stkPo stack, int32 *groups) {
  int32 pt = stackCount(stack);
  stackPush(def, stack);
  def->mark = False;

  int32 low = pt;

  argSpecPo ref;

  for (int32 ix = 0; (ref = findRef(def, defs, arity, ix)) != Null; ix++) {
    low = min(low, analyseRef(ref, defs, arity, stack, groups, low));
  }

  if (low < stackCount(stack)) {
    int32 group = (*groups)++;

    while (low < stackCount(stack)) {
      argSpecPo spec = stackPop(stack);
      spec->group = group;
    }
  }
  return low;
}

static void showGroups(ArgSpec defs[], int32 groups, int32 arity) {
  for (int32 gx = 0; gx < groups; gx++) {
    outMsg(logFile, "group %d: ", gx);
    char *sep = "";
    for (int32 ax = 0; ax < arity; ax++) {
      if (defs[ax].group == gx) {
        outMsg(logFile, "%sarg %F: %F", sep, defs[ax].dst, defs[ax].src);
        sep = ", ";
      }
    }
    outStr(logFile, "\n");
  }
  flushOut();
}

static void showDefs(ArgSpec defs[], int32 arity) {
  char *sep = "";
  for (int32 ax = 0; ax < arity; ax++) {
    outMsg(logFile, "%sarg %F: %F", sep, defs[ax].dst, defs[ax].src);
    sep = ", ";
  }
  outStr(logFile, "\n");
  flushOut();
}

static int32 sortArgSpecs(ArgSpec defs[], int32 arity) {
  int32 groups = 0;
  argSpecPo stackData[arity];
  Stack stack = {.top = 0, .stack = stackData};

#ifdef TRACEJIT
  if (traceJit >= detailedTracing) {
    showDefs(defs, arity);
  }
#endif

  argSpecPo def;
  while ((def = nextDef(defs, arity)) != Null) {
    analyseDef(def, defs, arity, &stack, &groups);
  }

#ifdef TRACEJIT
  if (traceJit >= detailedTracing) {
    showGroups(defs, groups, arity);
  }
#endif

  assert(stack.top==0);
  return groups;
}

// This is horrendous, but does not matter
static int32 groupSize(argSpecPo specs, int32 arity, int32 group) {
  int32 size = 0;
  for (int32 ix = 0; ix < arity; ix++) {
    if (specs[ix].group == group)
      size++;
  }
  return size;
}

static void collectGroup(argSpecPo args, int32 arity, int32 groupNo, argSpecPo *group) {
  int32 pt = 0;
  for (int32 ix = 0; ix < arity; ix++) {
    if (args[ix].group == groupNo) {
      group[pt++] = &args[ix];
    }
  }
}

void shuffleVars(assemCtxPo ctx, argSpecPo args, int32 arity, registerMap freeRegs, moveFunc mover, void *cl) {
  int32 groups = sortArgSpecs(args, arity);

  for (int32 gx = 0; gx < groups; gx++) {
    int32 grpSize = groupSize(args, arity, gx);
    argSpecPo group[grpSize];

    collectGroup(args, arity, gx, group);

    if (grpSize == 1) {
      FlexOp dst = group[0]->dst;

      if (!sameFlexOp(dst, group[0]->src)) {
        mover(ctx, group[0]->dst, group[0]->src, cl);
      }
      group[0]->group = -1;
    } else {
      mcRegister tmp = nxtAvailReg(freeRegs);
      FlexOp dst = group[0]->dst;

      mover(ctx,RG(tmp), dst, cl);

      for (int32 ix = 0; ix < grpSize - 1; ix++) {
        mover(ctx, group[ix]->dst, group[ix + 1]->src, cl);
      }

      mover(ctx, group[grpSize - 1]->src,RG(tmp), cl);
    }
  }
}
