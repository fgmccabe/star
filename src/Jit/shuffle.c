//
// Created by Francis McCabe on 9/25/25.
//

#include "shuffle.h"

typedef struct {
  int32 top;
  int32 size;
  argSpecPo *stack;
} Stack, *stkPo;

static int32 stackCount(stkPo stack) {
  return stack->top;
}

static argSpecPo stackPeek(stkPo stack, int32 ix) {
  if (ix >= stack->top)
    return Null;
  return stack->stack[ix];
}

static argSpecPo stackPop(stkPo stack) {
  if (stack->top == 0)
    return Null;
  return stack->stack[--stack->top];
}

static void stackPush(argSpecPo spec, stkPo stack) {
  assert(stack->top < stack->size);
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
  for (int32 ix = stackCount(stack); ix > 0; ix--) {
    argSpecPo stkRef = stackPeek(stack, ix - 1);

    if (affects(ref->dst, stkRef->src))
      return min(low, ix);
  }
  // look in definitions

  if (ref->mark)
    return min(low, analyseDef(ref, defs, arity, stack, groups));
  else
    return low;
}

argSpecPo findRef(argSpecPo def, ArgSpec defs[], int32 arity) {
  for (int32 ix = 0; ix < arity; ix++) {
    argSpecPo candidate = &defs[ix];
    if (candidate->mark && clobbers(candidate, def)) {
      return &defs[ix];
    }
  }

  return Null;
}

int32 analyseDef(argSpecPo def, ArgSpec defs[], int32 arity, stkPo stack, int32 *groups) {
  int32 pt = stackCount(stack);
  stackPush(def, stack);
  def->mark = False;

  // Is this reference already in the stack?
  for (int32 ix = pt; ix > 0; ix--) {
    // We dont need to check the definition we just pushed
    argSpecPo stkRef = stackPeek(stack, ix - 1);

    if (affects(def->dst, stkRef->src))
      return ix;
  }

  int low = pt;
  argSpecPo ref;
  while ((ref = findRef(def, defs, arity)) != Null) {
    low = min(low, analyseDef(ref, defs, arity, stack, groups));
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
    argSpecPo arg = &defs[ax];
    outMsg(logFile, "%sarg %F%s: %F", sep, arg->dst, (arg->mark ? "✓" : ""), arg->src);
    sep = ", ";
  }
  outStr(logFile, "\n");
  flushOut();
}

static int32 sortArgSpecs(ArgSpec defs[], int32 arity) {
  int32 groups = 0;
  argSpecPo stackData[arity];
  Stack stack = {.top = 0, .stack = stackData, .size = arity};

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

static logical marked(argSpecPo args, int32 arity, logical mark) {
  for (int32 ix = 0; ix < arity; ix++) {
    if (args[ix].mark != mark)
      return False;
  }
  return True;
}

static logical isClobbered(argSpecPo args, int32 arity, argSpecPo ref) {
  for (int32 ix = 0; ix < arity; ix++) {
    if (args[ix].mark && clobbers(&args[ix], ref))
      return True;
  }
  return False;
}

void shuffleVars(assemCtxPo ctx, argSpecPo args, int32 arity, registerMap *freeRegs, moveFunc mover) {
  int32 groups = sortArgSpecs(args, arity);

  assert(marked(args,arity,False));

  for (int32 gx = groups; gx > 0; gx--) {
    int32 grpSize = groupSize(args, arity, gx - 1);
    argSpecPo group[grpSize];

    collectGroup(args, arity, gx - 1, group);

    if (grpSize == 1) {
      assert(!group[0]->mark);
      assert(!isClobbered(args,arity,group[0]));
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

      mover(ctx,RG(tmp), dst, freeRegs);
      for (int32 ix = 0; ix < grpSize - 1; ix++) {
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
