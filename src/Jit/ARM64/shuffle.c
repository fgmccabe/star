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
  return affects(ref->src, def->dst);
}

static int32 analyseDef(argSpecPo def, ArgSpec defs[], int32 arity, stkPo stack, int32 *groups);

static int32 analyseRef(argSpecPo ref, ArgSpec defs[], int32 arity, stkPo stack, int32 *groups, int32 low) {
  // Is this reference already in the stack?
  for (int32 ix = 0; ix < stackCount(stack); ix++) {
    if (clobbers(ref, stackPeek(stack, ix)))
      return min(low, ix);
  }
  // look in definitions
  return min(low, analyseDef(ref, defs, arity, stack, groups));
}

argSpecPo findRef(argSpecPo def, ArgSpec defs[], int32 arity, int32 from) {
  for (int32 ix = from; ix < arity; ix++) {
    if (defs[ix].mark && clobbers(def, &defs[ix])) {
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
  argSpecPo ref = findRef(def, defs, arity, 0);

  for (int32 ix = 0; ref != Null; ix++, ref = findRef(def, defs, arity, ix)) {
    low = analyseRef(ref, defs, arity, stack, groups, low);
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

static void showDef(argSpecPo def) {
  outMsg(logFile, "arg %F: %F\n", def->dst, def->src);
}

static void showRegGroups(ArgSpec defs[], int32 groups, int32 arity) {
  for (int32 gx = 0; gx < groups; gx++) {
    outMsg(logFile, "group %d: ", gx);
    for (int32 ax = 0; ax < arity; ax++) {
      if (defs[ax].group == gx) {
        showDef(&defs[ax]);
      }
    }
    outMsg(logFile, "\n");
  }
  flushOut();
}

static int32 sortArgSpecs(ArgSpec defs[], int32 arity) {
  int32 groups = 0;
  argSpecPo stackData[arity];
  Stack stack = {.top = 0, .stack = stackData};

  argSpecPo def;
  while ((def = nextDef(defs, arity)) != Null) {
    analyseDef(def, defs, arity, &stack, &groups);
  }

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

void shuffleVars(assemCtxPo ctx, argSpecPo args, int32 arity, registerMap freeRegs) {
  int32 groups = sortArgSpecs(args, arity);

#ifdef TRACEJIT
  if (traceJit >= generalTracing) {
    showRegGroups(args, groups, arity);
  }
#endif

  for (int32 gx = 0; gx < groups; gx++) {
    if (groupSize(args, arity, gx) == 1) {
      for (int32 ax = 0; ax < arity; ax++) {
        if (args[ax].group == gx) {
          FlexOp dst = args[ax].dst;

          if (!sameFlexOp(dst, args[ax].src))
            move(ctx, args[ax].dst, args[ax].src, freeRegs);
          args[ax].group = -1;
        }
      }
    } else {
      check(False, "unsupported circular dependency");
    }
  }
}
