//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "jitOps.h"
#include "stackP.h"
#include "globals.h"
#include "signals.h"
#include "jitP.h"
#include "codeP.h"

/* Lower Star VM code to Arm64 code */

static retCode stackCheck(jitCompPo jit, methodPo mtd, int32 delta);
static const integer integerByteCount = (integer) sizeof(integer);

static retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun);
static retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun);
static retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun);

static retCode spillUpto(jitCompPo jit, integer depth);
static retCode loadStackIntoArgRegisters(jitCompPo jit, integer arity);

retCode jit_preamble(methodPo mtd, jitCompPo jit) {
  integer frameSize = lclCount(mtd) * integerByteCount + (integer) sizeof(StackFrame);
  if (!isInt32(frameSize))
    return Error;
  integer stkDelta = stackDelta(mtd) * integerByteCount;
  if (!isInt32(stkDelta))
    return Error;

  assemCtxPo ctx = assemCtx(jit);
  codeLblPo entry = defineLabel(ctx, ctx->pc);
  markEntry(jit, entry);
  int32 stkAdjustment = ALIGNVALUE(frameSize, 16);

  stp(FP, X30, PRX(SP, -sizeof(StackFrame)));
  mov(FP, RG(SP));
  str(PL, OF(FP, OffsetOf(StackFrame, pool)));
  if (stkAdjustment != 0)
    sub(SP, SP, IM(stkAdjustment));

  return stackCheck(jit, mtd, (int32) stkDelta);
}

retCode stackCheck(jitCompPo jit, methodPo mtd, int32 delta) {
  int32 stkMemOffset = OffsetOf(StackRecord, stkMem);
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo okLbl = defineLabel(ctx, undefinedPc);

  sub(X16, SP, IM(delta));
  ldr(X17, OF(X27, stkMemOffset));
  cmp(X16, RG(X17));
  bhi(okLbl);

  saveRegisters(ctx, nonSpillSet(codeArity(mtd)));

  mov(X0, RG(SB));
  mov(X1, IM(delta));
  mov(X2, IM((integer) mtd));
  tryRet(invokeCFunc3(jit, (Cfunc3) handleStackOverflow));
  mov(SB, RG(X0));

  restoreRegisters(ctx, nonSpillSet(codeArity(mtd)));

  setLabel(ctx, okLbl);
  return Ok;
}

retCode jit_postamble(methodPo mtd, jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);

  add(SP, FP, IM(sizeof(StackFrame)));
  mov(PL, OF(FP, OffsetOf(StackFrame, pool)));
  ldp(FP, LR, PSX(SP, 16));
  ret(LR);
  return Ok;
}

static vOperand popStkOp(jitCompPo jit) {
  verifyJitCtx(jit, 1, 0);
  return jit->vStack[--jit->vTop];
}

static void pushStkOp(jitCompPo jit, vOperand operand) {
  verifyJitCtx(jit, 0, 1);
  jit->vStack[jit->vTop++] = operand;
}

retCode jitInstructions(jitCompPo jit, insPo code, integer insCount, char *errMsg, integer msgLen) {
  retCode ret = Ok;
  assemCtxPo ctx = assemCtx(jit);

  for (integer pc = 0; pc < insCount; pc++) {
    switch (code[pc].op) {
      case Halt: {            // Stop execution
        integer errCode = code[pc].fst;
        ret = callIntrinsic(ctx, (libFun) star_exit, 1, IM(errCode));
        pc++;
        continue;
      }
      case Nop:            // No operation
        pc++;
        continue;
      case Abort:            // abort with message
        return Error;
      case Call: {            // Call <prog>
        int32 litNo = code[pc].fst;

        labelPo lbl = C_LBL(getMtdLit(jit->mtd, litNo));
        int32 arity = labelArity(lbl);

        spillUpto(jit, arity);    // Spill all stack arguments up until arity
        pc++;
        continue;
      }
      case OCall:            // OCall
      case Escape: {            // call C escape
        assemCtxPo ctx = assemCtx(jit);
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);

        spillUpto(jit, escapeArity(esc));    // Spill all stack arguments up until arity
        loadStackIntoArgRegisters(jit, escapeArity(esc));
        codeLblPo escLbl = defineLabel(ctx, (integer) escapeFun(esc));
        bl(escLbl);
        pc++;
        continue;
      }
      case TCall:            // TCall <prog>
      case TOCall:            // TOCall
      case Entry:            // locals definition
        return Error;
      case Ret:            // return
      case Block:            // block of instructions
      case Break: {            // leave block
        assemCtxPo ctx = assemCtx(jit);
        codeLblPo tgt = getLblByPc(&code[pc], code[pc].alt, jit);

        assert(tgt != Null);

        b(tgt);
        pc++;
        continue;
      }
      case Result:            // return value out of block
      case Loop: {            // jump back to start of block
        assemCtxPo ctx = assemCtx(jit);
        codeLblPo tgt = getLblByPc(&code[pc], code[pc].alt, jit);

        assert(tgt != Null);

        b(tgt);
        pc++;
        continue;
      }
      case Drop: {            // drop top of stack
        verifyJitCtx(jit, 1, 0);
        jit->vTop--;
        pc++;
        continue;
      }
      case Dup: {            // duplicate top of stack
        verifyJitCtx(jit, 1, 1);
        jit->vStack[jit->vTop] = jit->vStack[jit->vTop - 1];
        jit->vTop++;
        pc++;
        continue;
      }
      case Rot: {           // Pull up nth element of stack
        int32 height = code[pc].fst;
        check(height >= 0 && height <= jit->vTop, "rotation amount");
        vOperand top = jit->vStack[jit->vTop];
        for (int32 ix = 0; ix < height - 1; ix++) {
          jit->vStack[jit->vTop - ix] = jit->vStack[jit->vTop - height - ix];
        }
        jit->vStack[jit->vTop - height] = top;
        pc++;
        continue;
      }
      case Rst: {            // reset stack height to a fixed height
        int32 height = code[pc].fst;
        check(height >= 0 && height <= jit->vTop, "reset alignment");
        jit->vTop = height;
        pc++;
        continue;
      }
      case Pick:            // adjust stack to n depth, using top k elements
      case Fiber:            // Create new fiber
      case Spawn:            // spawn a new task
      case Suspend:            // suspend fiber
      case Resume:            // resume fiber
      case Retire:            // retire a fiber
      case Underflow:            // underflow from current stack
      case Try:            // a try-catch block
      case EndTry:            // end try
      case TryRslt:            // end try with a  result
      case Throw:            // Invoke a continuation
      case LdV: {            // Place a void value on stack
        verifyJitCtx(jit, 1, 0);

        vOperand vdOp = {.loc=engineSymbol, .address=voidEnum};
        jit->vStack[jit->vTop++] = vdOp;
        pc++;
        continue;
      }
      case LdC: {            // load literal from constant pool
        verifyJitCtx(jit, 1, 0);
        int32 litNo = code[pc].fst;
        vOperand lclOp = {.loc=constant, .ix=litNo};
        jit->vStack[jit->vTop++] = lclOp;
        pc++;
        continue;
      }
      case LdA: {            // load stack from args[xx]
        verifyJitCtx(jit, 1, 0);
        int32 argNo = code[pc].fst;
        vOperand argOp = {.loc=argument, .ix=argNo};

        jit->vStack[jit->vTop++] = argOp;
        pc++;
        continue;
      }
      case LdL: {            // load stack from local[xx]
        verifyJitCtx(jit, 1, 0);
        int32 lclNo = code[pc].fst;
        vOperand lclOp = {.loc=local, .ix=lclNo};
        jit->vStack[jit->vTop++] = lclOp;
        pc++;
        continue;
      }
      case StL:            // store tos to local[xx]
        return Error;
      case StV: {           // clear a local to void
        int32 lclNo = code[pc].fst;
        return Error;
      }
      case TL:            // copy tos to local[xx]
        return Error;
      case LdS:            // lift a value from the stack
        return Error;
      case LdG: {            // load a global variable
        verifyJitCtx(jit, 1, 0);
        int32 litNo = code[pc].fst;
        vOperand lclOp = {.loc=global, .ix=litNo};
        jit->vStack[jit->vTop++] = lclOp;
        pc++;
        continue;
      }
      case StG:            // store into a global variable
        return Error;
      case TG:            // copy into a global variable
        return Error;
      case Sav:            // create a single assignment variable
        return Error;
      case LdSav:            // derefence a sav, break if not set
        return Error;
      case TstSav:            // test a sav, return a logical
        return Error;
      case StSav:            // store a value into a single assignment variable
        return Error;
      case TSav:            // update single assignment variable leave value on stack
        return Error;
      case Cell:            // create R/W cell
      case Get:            // access a R/W cell
      case Assign:            // assign to a R/W cell
      case CLbl:            // T,Lbl --> test for a data term, break if not lbl
      case CLit:            // T,lit --> test for a literal value, break if not
      case Nth:            // T --> el, pick up the nth element
        return Error;
      case StNth:            // T el --> store in nth element
        return Error;
      case If:            // break if true
      case IfNot:            // break if false
      case Case:            // T --> T, case <Max>
      case IndxJmp:            // check and jump on index
      case IAdd: {           // L R --> L+R
        verifyJitCtx(jit, 1, 0);
        vOperand a1 = popStkOp(jit);
        vOperand a2 = popStkOp(jit);

//  add(a1, a2, jit->assemCtx);
        pushStkOp(jit, a1);
        pc++;
        continue;
      }
      case ISub:            // L R --> L-R
      case IMul:            // L R --> L*R
      case IDiv:            // L R --> L/R
      case IMod:            // L R --> L%R
      case IAbs:            // L --> abs(L)
      case IEq:            // L R --> L==R
      case ILt:            // L R --> L<R
      case IGe:            // L R --> L>=R
      case ICmp:            // L R --> break if not same integer
      case CEq:            // L R --> L==R
      case CLt:            // L R --> L<R
      case CGe:            // L R --> L>=R
      case CCmp:            // L R --> break if not same character
      case BAnd:            // L R --> L&R
      case BOr:            // L R --> L|R
      case BXor:            // L R --> L^R
      case BLsl:            // L R --> L<<R
      case BLsr:            // L R --> L>>R
      case BAsr:            // L R --> L>>>R
      case BNot:            // L --> ~L
      case FAdd:            // L R --> L+R
      case FSub:            // L R --> L-R
      case FMul:            // L R --> L*R
      case FDiv:            // L R --> L/R
      case FMod:            // L R --> L%R
      case FAbs:            // L --> abs(L)
      case FEq:            // L R e --> L==R
      case FLt:            // L R --> L<R
      case FGe:            // L R --> L>=R
      case FCmp:            // L R --> branch if not same floating point
      case Alloc:            // new structure, elements from stack
        return Error;
      case Closure:            // allocate a closure
        return Error;
      case Cmp:            // t1 t2 --> , branch to offset if not same literal
      case Frame:            // frame instruction
      case dBug:            // debugging prefix
      default:
        return Error;
    }
  }

  return ret;
}

retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun) {
  return Error;
}

retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun) {
  return Error;
}

retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun) {
  return Error;
}

retCode spillUpto(jitCompPo jit, integer depth) {
  assemCtxPo ctx = assemCtx(jit);

  for (integer ix = 0; ix < jit->vTop - depth; ix++) {
    operandPo entry = &jit->vStack[ix];

    switch (entry->loc) {
      case argument:
      case local:
      case constant:
      case global:
        continue;
      case mcReg: {
        integer off = allocateLocal(jit, -1, -1, spilledVar);
        armReg Rg = entry->mcLoc.reg;
        str(Rg, OF(FP, off));
        entry->loc = local;
        entry->ix = off;
        continue;
      }
      default:
        check(False, "illegal source loc on stack");
        return Error;
    }
  }
  return Ok;
}

typedef struct {
  armReg src;
  armReg dst;
} RgMvSpec;

static retCode shuffleRegisters(jitCompPo jit, RgMvSpec *specs, int16 mvTop);

retCode loadStackIntoArgRegisters(jitCompPo jit, integer arity) {
  assemCtxPo ctx = assemCtx(jit);
  RgMvSpec specs[32];
  int16 mvTop = 0;

  assert(jit->vTop >= arity);

  armReg argRg = X0;

  for (integer ix = 0; ix < arity; ix++, argRg++) {
    operandPo entry = &jit->vStack[jit->vTop - ix];

    switch (entry->loc) {
      case argument:
      case local:
      case constant:
      case global:
        continue;
      case mcReg: {
        armReg Rg = entry->mcLoc.reg;
        if (Rg == argRg)
          continue;
        else if (Rg > argRg) {
          mov(argRg, RG(Rg));
          continue;
        } else {
          specs[mvTop].src = Rg;
          specs[mvTop].dst = argRg;
          mvTop++;
          continue;
        }
      }
      default:
        check(False, "illegal source loc on stack");
        return Error;
    }
  }

  if (mvTop > 0)
    return shuffleRegisters(jit, specs, mvTop);
  return Ok;
}

retCode shuffleRegisters(jitCompPo jit, RgMvSpec *specs, int16 mvTop) {
  check(False, "not implemented yet");

  return Error;
}

retCode allocateStructure(clssPo clss, FlexOp amnt, armReg dst, jitCompPo jit) {
  uint32 currOff = OffsetOf(HeapRecord, curr);
  uint32 limitOff = OffsetOf(HeapRecord, limit);
  assemCtxPo ctx = assemCtx(jit);
  armReg glbHeap = findFreeReg(jit);
  armReg scratch = findFreeReg(jit);
  armReg limit = findFreeReg(jit);

  codeLblPo okLbl = newLabel(ctx);
  codeLblPo endLbl = newLabel(ctx);

  ldr(glbHeap, IM((uinteger) globalHeap));
  ldr(dst, OF(glbHeap, currOff));           // pick up current heap top
  add(scratch, dst, amnt);
  ldr(limit, OF(glbHeap, limitOff));        // check against limit
  cmp(limit, RG(scratch));
  bgt(okLbl);

  // Invoke out of line allocator


  b(endLbl);

  setLabel(ctx, okLbl);
  str(scratch, OF(glbHeap, currOff));         // record new heap top
  mov(scratch, IM((uinteger) clss));
  str(scratch, OF(dst, OffsetOf(TermRecord, clss))); // record class of new structure

  releaseReg(jit, glbHeap);
  releaseReg(jit, scratch);
  releaseReg(jit, limit);
  setLabel(ctx, endLbl);
  return Ok;
}
