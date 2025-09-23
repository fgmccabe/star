//
// Created by Francis McCabe on 9/8/24.
//

#include "macros.h"

#include "jitP.h"
#include "lifo.h"

registerMap defltAvailRegSet() {
  return 1u << X0 | 1u << X1 | 1u << X2 | 1u << X3 | 1u << X4 | 1u << X5 | 1u << X6 | 1u << X7 | 1u << X8 | 1u << X9 |
         1u << X10;
}

registerMap emptyRegSet() {
  return 0;
}

registerMap nonSpillSet(integer arity) {
  registerMap set = emptyRegSet();
  switch (arity) {
    default:
    case 9:
      set = addReg(set, X8);
    case 8:
      set = addReg(set, X7);
    case 7:
      set = addReg(set, X6);
    case 6:
      set = addReg(set, X5);
    case 5:
      set = addReg(set, X4);
    case 4:
      set = addReg(set, X3);
    case 3:
      set = addReg(set, X2);
    case 2:
      set = addReg(set, X1);
    case 1:
      set = addReg(set, X0);
    case 0:
      return set;
  }
}

registerMap allocReg(registerMap from, armReg Rg) {
  check((from & ((uint64)1u << Rg)) != 0, "register not free");
  return (from & (~((uint64) 1u << Rg)));
}

registerMap freeReg(registerMap from, armReg Rg) {
  check((from & ((uint64)1u << Rg)) == 0, "register already free");
  return (from | ((uint64) 1u << Rg));
}

registerMap dropReg(registerMap map, armReg Rg) {
  return (map & (~((uint64) 1u << Rg)));
}

registerMap addReg(registerMap from, armReg Rg) {
  return (from | ((uint64) 1u << Rg));
}

logical isRegInMap(registerMap from, armReg Rg) {
  return ((from & ((uint64) 1u << Rg)) != 0);
}

armReg nxtAvailReg(registerMap from) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = (uint64) 1u << ix;
    if ((from & mask) != 0)
      return ix;
  }
  return XZR;
}

void processRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 0; ix < 64u; ix++) {
    uint64 mask = (uint64) 1u << ix;
    if ((set & mask) != 0)
      proc((armReg) ix, cl);
  }
}

void revProcessRegisterMap(registerMap set, regProc proc, void *cl) {
  for (uint32 ix = 64u; ix > 0;) {
    ix--;
    uint64 mask = (uint64) 1u << ix;
    if ((set & mask) != 0)
      proc((armReg) ix, cl);
  }
}

static void svRegisters(assemCtxPo ctx, registerMap regs, armReg Rg) {
  armReg nxt = nxtAvailReg(regs);

  if (nxt == XZR) {
    if (Rg != XZR)
      stp(Rg, XZR, PRX(SP, -16));
  } else if (Rg == XZR)
    svRegisters(ctx, dropReg(regs, nxt), nxt);
  else {
    stp(Rg, nxt, PRX(SP, -16));
    svRegisters(ctx, dropReg(regs, nxt), XZR);
  }
}

void saveRegisters(assemCtxPo ctx, registerMap regs) {
  svRegisters(ctx, regs, XZR);
}

static void restRegisters(assemCtxPo ctx, registerMap regs, armReg Rg) {
  armReg nxt = nxtAvailReg(regs);

  if (nxt == XZR) {
    if (Rg != XZR)
      ldp(Rg, XZR, PSX(SP, 16));
  } else if (Rg == XZR)
    restRegisters(ctx, dropReg(regs, nxt), nxt);
  else {
    restRegisters(ctx, dropReg(regs, nxt), XZR);
    ldp(Rg, nxt, PSX(SP, 16));
  }
}

void restoreRegisters(assemCtxPo ctx, registerMap regs) {
  restRegisters(ctx, regs, XZR);
}

static char *regNames[] = {
  "X0", "X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10", "X11", "X12", "X13", "X14", "X15",
  "X16", "X17", "X18", "X19", "X20", "X21", "X22", "X23", "X24", "X25", "X26", "X27", "X28", "FP", "LR", "SP"
};

static char *extentNames[] = {"uxtb", "uxth", "uxtw", "uxtx", "sxtb", "sxth", "sxtw", "sxtx"};

static char *shiftModeName[] = {"lsl", "lsr", "asr", "ror"};

static armReg argRegs[] = {X0, X1, X2, X3, X4, X5, X6, X7};

void showReg(armReg rg, void *cl) {
  outMsg((ioPo) cl, "%s ", regNames[(uint8) rg]);
}

retCode showFlexOp(ioPo out, FlexOp op) {
  switch (op.mode) {
    case reg:
      return outMsg(out, "%s", regNames[(uint8) op.reg]);
    case sOff:
      return outMsg(out, "%s[%d]", regNames[op.reg], op.immediate);
    case preX:
      return outMsg(out, "[%s, #%x]!", regNames[op.reg], op.immediate);
    case postX:
      return outMsg(out, "[%s], #%x", regNames[op.reg], op.immediate);
    case extnd:
      return outMsg(out, "[%s, %s %s], #%x", regNames[op.reg], regNames[op.rgm], extentNames[op.ext], op.immediate);
    case imm:
      return outMsg(out, "%x", op.immediate);
    case shft:
      return outMsg(out, "[%s, %s %s #%x]", regNames[op.reg], regNames[op.rgm], shiftModeName[op.shift], op.immediate);
    case fp:
      return outMsg(out, "%s", regNames[(uint8) op.reg]);
    case pcRel:
      return outMsg(out, "pc+", op.immediate);
  }
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

void dRegisterMap(registerMap regs) {
  outMsg(logFile, "registers: {");
  processRegisterMap(regs, showReg, logFile);
  outMsg(logFile, "}\n");
  flushOut();
}

// Implement a specific topological sort for register references

typedef struct {
  FlexOp op;
  armReg argReg;
  logical mark;
  int32 group;
} ArgSpec, *argSpecPo;

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

static logical dependsOn(argSpecPo def, argSpecPo ref) {
  return usesReg(ref->op, def->argReg);
}

static int32 analyseDef(argSpecPo def, ArgSpec defs[], int32 arity, stkPo stack, int32 *groups);

static int32 analyseRef(argSpecPo ref, ArgSpec defs[], int32 arity, stkPo stack, int32 *groups, int32 low) {
  // Is this reference already in the stack?
  for (int32 ix = 0; ix < stackCount(stack); ix++) {
    if (dependsOn(ref, stackPeek(stack, ix)))
      return min(low, ix);
  }
  // look in definitions
  for (integer ix = 0; ix < arity; ix++) {
    if (defs[ix].mark && dependsOn(ref, &defs[ix])) {
      return min(low, analyseDef(&defs[ix], defs, arity, stack, groups));
    }
  }
  return low;
}

argSpecPo findRef(argSpecPo def, ArgSpec defs[], int32 arity, int32 from) {
  for (int32 ix = from; ix < arity; ix++) {
    if (defs[ix].mark && dependsOn(&defs[ix], def)) {
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

static void showRegGroups(ArgSpec defs[], int32 groups, int32 arity) {
  for (int32 gx = 0; gx < groups; gx++) {
    outMsg(logFile, "group %d: ", gx);
    char *sep = "";
    for (int32 ax = 0; ax < arity; ax++) {
      if (defs[ax].group == gx) {
        outMsg(logFile, "%s %s", sep, regNames[(uint8) defs[ax].argReg]);
        for (int32 ix = 0; ix < arity; ix++) {
          argSpecPo def = &defs[ix];
          if (dependsOn(def, &defs[ax])) {
            outMsg(logFile, " <- %s ", regNames[(uint8) def->argReg]);
          }
        }
        sep = ",";
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

static void intrinsicArgs(assemCtxPo ctx, argSpecPo args, int32 arity) {
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
          armReg destReg = args[ax].argReg;

          if (!sameFlexOp(RG(destReg), args[ax].op))
            mov(args[ax].argReg, args[ax].op);
          args[ax].group = -1;
        }
      }
    } else
      check(False, "invalid register group size");
  }
}

retCode callIntrinsic(assemCtxPo ctx, registerMap saveMap, runtimeFn fn, int32 arity, ...) {
  va_list args;
  va_start(args, arity); /* start the variable argument sequence */

  ArgSpec operands[arity];

  for (int32 ix = 0; ix < arity; ix++) {
    operands[ix] = (ArgSpec){.op = (FlexOp) va_arg(args, FlexOp), .argReg = argRegs[ix], .mark = True, .group = -1};
  }
  va_end(args);

  saveRegisters(ctx, saveMap);

  intrinsicArgs(ctx, operands, arity);

  mov(X16, IM((integer) fn));
  blr(X16);
  restoreRegisters(ctx, saveMap);
  return Ok;
}

retCode loadCGlobal(assemCtxPo ctx, armReg reg, void *address) {
  mov(reg, IM((integer) address));
  ldr(reg, OF(reg, 0));
  return Ok;
}
