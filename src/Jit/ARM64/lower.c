//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include "lowerP.h"
#include "stackP.h"
#include "globals.h"
#include "constants.h"
#include "jitP.h"
#include "formioP.h"

/* Lower Star VM code to Arm64 code */

static retCode stackCheck(jitCompPo jit, methodPo mtd, int32 delta);
static int32 pointerSize = sizeof(integer);

static int32 argOffset(int32 argNo) {
  return argNo * pointerSize;
}

static retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun);
static retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun);
static retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun);

static retCode bail(jitCompPo jit, char *msg, ...);

static retCode loadStackIntoArgRegisters(jitCompPo jit, uint32 arity);
static void setJitHeight(jitCompPo jit, int32 height);

static retCode getIntVal(jitCompPo jit, armReg rg);
static retCode mkIntVal(jitCompPo jit, armReg rg);
#define SSP (X28)

static retCode jitError(jitCompPo jit, char *msg, ...);

typedef struct jitBlock_ *jitBlockPo;

typedef struct jitBlock_ {
  int32 height;
  int32 startPc;
  codeLblPo breakLbl;
  codeLblPo loopLbl;
  jitBlockPo parent;
} JitBlock;

static codeLblPo breakLabel(jitBlockPo block, int32 tgt);
static codeLblPo loopLabel(jitBlockPo block, int32 tgt);

retCode jit_preamble(methodPo mtd, jitCompPo jit) {
  integer frameSize = lclCount(mtd) * pointerSize + (integer) sizeof(StackFrame);
  if (!isInt32(frameSize))
    return Error;
  integer stkDelta = stackDelta(mtd) * pointerSize;
  if (!isInt32(stkDelta))
    return Error;

  assemCtxPo ctx = assemCtx(jit);
  codeLblPo entry = defineLabel(ctx, ctx->pc);
  markEntry(jit, entry);
  int32 stkAdjustment = ALIGNVALUE(frameSize, 16);

  stp(FP, X30, PRX(SP, -sizeof(StackFrame)));
  mov(FP, RG(SP));
  str(PLE, OF(FP, OffsetOf(StackFrame, prog)));
  if (stkAdjustment != 0)
    sub(SP, SP, IM(stkAdjustment));

  return stackCheck(jit, mtd, (int32) stkDelta);
}

retCode stackCheck(jitCompPo jit, methodPo mtd, int32 delta) {
  int32 stkMemOffset = OffsetOf(StackRecord, stkMem);
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo okLbl = defineLabel(ctx, undefinedPc);

  sub(X16, SSP, IM(delta));
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

retCode jit_postamble(jitCompPo jit) {
  assemCtxPo ctx = assemCtx(jit);

  ldr(SSP, OF(FP, OffsetOf(StackFrame, args)));
  mov(X16, IM(argCount(jit->mtd) * pointerSize));
  add(SSP, SSP, RG(X16));

  add(SSP, FP, IM(sizeof(StackFrame)));
  mov(PLE, OF(FP, OffsetOf(StackFrame, prog)));
  ldp(FP, LR, PSX(SSP, 16));
  ret(LR);
  return Ok;
}

static armReg popStkOp(jitCompPo jit) {
  armReg tgt = findFreeReg(jit);
  assemCtxPo ctx = assemCtx(jit);
  int32 stackOffset = jit->currSPOffset;
  jit->currSPOffset += pointerSize;
  ldr(tgt, OF(SSP, stackOffset));
  jit->vTop--;

  return tgt;
}

static armReg topStkOp(jitCompPo jit) {
  armReg tgt = findFreeReg(jit);
  assemCtxPo ctx = assemCtx(jit);
  int32 stackOffset = jit->currSPOffset;
  ldr(tgt, OF(SSP, stackOffset));

  return tgt;
}

static void pushStkOp(jitCompPo jit, armReg src) {
  assemCtxPo ctx = assemCtx(jit);

  int32 stackOffset = jit->currSPOffset -= pointerSize;
  str(src, OF(SSP, stackOffset));
  jit->vTop++;
}

static retCode jitBlock(jitCompPo jit, jitBlockPo parent, int32 height, insPo code,
                        int32 from, int32 startPc, int32 endPc) {
  retCode ret = Ok;
  assemCtxPo ctx = assemCtx(jit);

  JitBlock block = {
    .startPc = from,
    .height = height,
    .breakLbl = newLabel(ctx),
    .loopLbl = currentPcLabel(ctx),
    .parent = parent
  };

  for (int32 pc = startPc; ret == Ok && pc < endPc; pc++) {
    switch (code[pc].op) {
      case Halt: {            // Stop execution
        integer errCode = code[pc].fst;
        ret = callIntrinsic(ctx, (runtimeFn) star_exit, 1, IM(errCode));
        continue;
      }
      case Nop:            // No operation
        continue;
      case Abort:            // abort with message
        return Error;
      case Call: {            // Call <prog>
        labelPo nProg = C_LBL(getConstant(code[pc].fst));

        // pick up the pointer to the method
        mov(X16, IM((integer) nProg));
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bail(jit, "Function %T not defined", nProg);

        setLabel(ctx, haveMtd);

        codeLblPo returnPc = newLabel(ctx);
        adr(X0, returnPc);
        str(X0, OF(FP, OffsetOf(StackFrame, pc)));

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit)));
        br(X16);

        setLabel(ctx, returnPc);
        continue;
      }
      case XCall: {            // Call <prog>, with catch
        labelPo nProg = C_LBL(getConstant(code[pc].fst));

        // pick up the pointer to the method
        mov(X16, IM((integer) nProg));
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bail(jit, "Function %T not defined", nProg);

        setLabel(ctx, haveMtd);

        codeLblPo returnPc = newLabel(ctx);
        adr(X0, returnPc);
        str(X0, OF(FP, OffsetOf(StackFrame, pc)));

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit)));
        br(X16);

        codeLblPo catch = breakLabel(&block, pc + code[pc].alt + 1);
        if (catch == Null)
          return jitError(jit, "not in try scope");

        emitLblRef(ctx, catch);

        setLabel(ctx, returnPc);
        continue;
      }

      case OCall:            // OCall
      case Escape: {            // call C escape
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        loadStackIntoArgRegisters(jit, arity);
        codeLblPo escLbl = defineLabel(ctx, (integer) escapeFun(esc));
        bl(escLbl);
        pc++;
        continue;
      }
      case XEscape:
      case XOCall:
      case TCall:            // TCall <prog>
      case TOCall:            // TOCall
        return Error;
      case Entry: {           // locals definition
        int32 locals = code[pc].fst;
        jit->localDepth = locals;
        setJitHeight(jit, 0);
        assert(locals >= 0);
        armReg vd = findFreeReg(jit);
        mov(vd, IM((integer) voidEnum));
        stp(LR, FP, PRX(SSP, -2 * pointerSize));
        mov(FP, PRX(SSP, -pointerSize));

        if (locals < 8) {
          for (int32 ix = 0; ix < locals; ix++) {
            str(vd, OF(FP, (ix - locals) * sizeof(integer)));
          }
        } else {
          // Build a loop
          armReg cx = findFreeReg(jit);
          mov(cx, IM(-locals));
          codeLblPo start = currentPcLabel(ctx);
          str(vd, EX2(FP, cx, S_XTX, 3));
          sub(cx, cx, IM(1));
          bne(start);
          releaseReg(jit, cx);
        }

        releaseReg(jit, vd);
        pc++;
        continue;
      }
      case Ret: {           // return
        ldr(X0, RG(SSP));
        // Pick up the currently running program
        ldr(X16, OF(FP, OffsetOf(StackFrame, prog)));
        ldr(X17, OF(X16, OffsetOf(MethodRec, lbl)));
        ldr(X17, OF(X17, OffsetOf(LabelRecord, arity)));
        sub(SSP, FP, RG(X17));
        sub(FP, FP, IM(sizeof(StackFrame)));
        pushStkOp(jit, X0);
        ldr(X0, OF(FP, OffsetOf(StackFrame, pc)));
        br(X0);
      }
      case XRet:
        return Error;
      case Block: {            // block of instructions
        int32 blockLen = code[pc].alt;

        ret = jitBlock(jit, &block, code[pc].fst, code, pc, pc + 1, pc + blockLen);
        pc += blockLen;
        continue;
      }
      case Break: {            // leave block
        insPo blockStart = &code[pc + code[pc].alt];

        assert(blockStart->op == Block);

        codeLblPo tgt = getJitLbl(jit, blockStart + blockStart->alt);

        assert(tgt != Null);

        b(tgt);
        pc++;
        continue;
      }
      case Result:            // return value out of block
      case Loop: {            // jump back to start of block
        codeLblPo tgt = getJitLbl(jit, &code[pc + code[pc].alt]);

        assert(tgt != Null);

        b(tgt);
        pc++;
        continue;
      }
      case Drop: {            // drop top of stack
        jit->currSPOffset += pointerSize;
        pc++;
        continue;
      }
      case Dup: {            // duplicate top of stack
        armReg tgt = findFreeReg(jit);
        int32 stackOffset = jit->currSPOffset;
        ldr(tgt, OF(SSP, stackOffset));
        jit->currSPOffset -= pointerSize;
        str(tgt, OF(SSP, stackOffset));
        releaseReg(jit, tgt);

        pc++;
        continue;
      }
      case Rot: {           // Pull up nth element of stack
        int32 height = code[pc].fst;
        check(height >= 0 && height <= jit->vTop, "rotation amount");
        armReg swp = findFreeReg(jit);
        armReg src = findFreeReg(jit);

        ldr(swp, OF(SSP, jit->currSPOffset));

        for (int32 ix = 1; ix <= height; ix++) {
          ldr(src, OF(SSP, jit->currSPOffset + (ix * sizeof(integer))));
          str(src, OF(SSP, jit->currSPOffset + ((ix - 1) * sizeof(integer))));
        }
        str(swp, OF(SSP, jit->currSPOffset + height * sizeof(integer)));
        releaseReg(jit, swp);
        releaseReg(jit, src);
        pc++;
        continue;
      }
      case Rst: {            // reset stack height to a fixed height
        int32 height = code[pc].fst;
        check(height >= 0 && height <= jit->vTop, "reset alignment");
        setJitHeight(jit, height);
        pc++;
        continue;
      }
      case Pick: {            // adjust stack to n depth, using top k elements
        int32 height = code[pc].fst;
        int32 keep = code[pc].alt;
//        check(height >= 0 && height <= jit->vTop, "reset alignment");
//        check(keep >= 0 && keep <= jit->vTop - height, "keep depth more than available");
//        for (int32 ix = 0; ix < keep; ix++) {
//          jit->vStack[height + ix] = jit->vStack[jit->vTop - keep + ix];
//        }
//        jit->vTop = height + keep;
        pc++;
        continue;
      }
      case Fiber:            // Create new fiber
      case Suspend:            // suspend fiber
      case Resume:            // resume fiber
      case Retire:            // retire a fiber
      case Underflow:            // underflow from current stack
      case LdV: {            // Place a void value on stack
        mov(X0, IM((integer) voidEnum));
        pushStkOp(jit, X0);
        pc++;
        continue;
      }
      case LdC: {            // load literal from constant pool
        int32 key = code[pc].fst;
        termPo lit = getConstant(key);

        mov(X0, IM((integer) lit));
        pushStkOp(jit, X0);

        pc++;
        continue;
      }
      case LdA: {            // load stack from args[xx]
        int32 argNo = code[pc].fst;
        armReg rg = findFreeReg(jit);
        ldr(rg, OF(FP, OffsetOf(StackFrame, args)));
        ldr(rg, OF(rg, argOffset(argNo)));
        pushStkOp(jit, rg);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case LdL: {            // load stack from local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg rg = findFreeReg(jit);
        ldr(rg, OF(FP, OffsetOf(StackFrame, args)));
        ldr(rg, OF(rg, offset));
        pushStkOp(jit, rg);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case StL: {            // store tos to local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vl = popStkOp(jit);
        armReg rg = findFreeReg(jit);
        ldr(rg, OF(FP, OffsetOf(StackFrame, args)));
        str(vl, OF(rg, offset));
        releaseReg(jit, vl);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case StV: {           // clear a local to void
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vd = findFreeReg(jit);
        mov(vd, IM((integer) voidEnum));
        armReg rg = findFreeReg(jit);
        ldr(rg, OF(FP, OffsetOf(StackFrame, args)));
        str(vd, OF(rg, offset));
        releaseReg(jit, vd);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case TL: {           // copy tos to local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vl = topStkOp(jit);
        armReg vr = findFreeReg(jit);
        ldr(vr, OF(FP, OffsetOf(StackFrame, args)));
        str(vl, OF(vr, offset));
        releaseReg(jit, vr);
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case LdG: {            // load a global variable
        verifyJitCtx(jit, 1, 0);
        int32 litNo = code[pc].fst;
        vOperand lclOp = {.loc=global, .ix=litNo};
//        jit->vStack[jit->vTop++] = lclOp;
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
      case CLit: {            // T,lit --> test for a literal value, break if not
        termPo lit = getConstant(code[pc].fst);
        armReg rg = findFreeReg(jit);
        mov(rg, IM((integer) lit));
        armReg st = popStkOp(jit);

      }

      case Nth:            // T --> el, pick up the nth element
        return Error;
      case StNth:            // T el --> store in nth element
        return Error;
      case If:            // break if true
      case IfNot:            // break if false
      case Case:            // T --> T, case <Max>
      case IndxJmp:            // check and jump on index
      case IAdd: {           // L R --> L+R
        armReg a1 = popStkOp(jit);
        armReg a2 = popStkOp(jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        add(a1, a2, RG(a2));

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case ISub: {            // L R --> L-R
        armReg a1 = popStkOp(jit);
        armReg a2 = popStkOp(jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        sub(a1, a1, RG(a2));

        mkIntVal(jit, a1);
        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case IMul: {            // L R --> L*R
        armReg a1 = popStkOp(jit);
        armReg a2 = popStkOp(jit);

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        mul(a1, a2, a2);

        mkIntVal(jit, a1);

        pushStkOp(jit, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case IDiv: {           // L R --> L/R
      }
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
      case maxOpCode:
      case illegalOp:
        return Error;
    }
  }

  return ret;
}

retCode jitInstructions(jitCompPo jit, insPo code, integer insCount, char *errMsg, integer msgLen) {
  return jitBlock(jit, Null, 0, code, 0, 0, insCount);
}

retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun) {
  assemCtxPo ctx = assemCtx(jit);

  codeLblPo lbl = defineLabel(ctx, (integer) fun);
  bl(lbl);
  return Ok;
}

retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun) {
  return Error;
}

retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun) {
  return Error;
}

retCode loadStackIntoArgRegisters(jitCompPo jit, uint32 arity) {
  assemCtxPo ctx = assemCtx(jit);
  assert(arity < 9);

  for (uint32 ix = 0; ix < arity; ix++) {
    ldr((armReg) (X0 + ix), OF(SSP, jit->currSPOffset));
    jit->currSPOffset += pointerSize;
  }
  return Ok;
}

static void setJitHeight(jitCompPo jit, int32 height) {
  jit->currSPOffset = jit->localDepth - height;
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

retCode getIntVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  asr(rg, rg, IM(2));
  return Ok;
}

retCode mkIntVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  lsl(rg, rg, IM(2));
  orr(rg, rg, IM(intTg));
  return Ok;
}

codeLblPo breakLabel(jitBlockPo block, int32 tgt) {
  while (block != Null) {
    if (block->startPc == tgt)
      return block->breakLbl;
    else
      block = block->parent;
  }
  return Null;
}

codeLblPo loopLabel(jitBlockPo block, int32 tgt) {
  while (block != Null) {
    if (block->startPc == tgt)
      return block->loopLbl;
    else
      block = block->parent;
  }
  return Null;
}

retCode jitError(jitCompPo jit, char *msg, ...) {
  char buff[MAXLINE];
  strBufferPo f = fixedStringBuffer(buff, NumberOf(buff));

  va_list args;      /* access the generic arguments */
  va_start(args, msg);    /* start the variable argument sequence */

  __voutMsg(O_IO(f), msg, args);  /* Display into the string buffer */

  va_end(args);
  outByte(O_IO(f), 0);                /* Terminate the string */

  closeIo(O_IO(f));

  strMsg(jit->errMsg, jit->msgLen, RED_ESC_ON "%s"RED_ESC_OFF, buff);
  return Error;
}

retCode bail(jitCompPo jit, char *msg, ...) {
  return callIntrinsic(assemCtx(jit), (runtimeFn) star_exit, 1, IM(100));

  /* char buff[MAXLINE]; */
  /* strBufferPo f = fixedStringBuffer(buff, NumberOf(buff)); */

  /* va_list args;      /\* access the generic arguments *\/ */
  /* va_start(args, msg);    /\* start the variable argument sequence *\/ */

  /* __voutMsg(O_IO(f), msg, args);  /\* Display into the string buffer *\/ */

  /* va_end(args); */
  /* outByte(O_IO(f), 0);                /\* Terminate the string *\/ */

  /* closeIo(O_IO(f)); */

  /* assemCtxPo ctx = assemCtx(jit);   */
  /* integer conString = stringConstant(ctx,buff,uniStrlen(buff)); */
  /* ldr(X0,IM(conString)); */

  /* return callIntrinsic(ctx, (runtimeFn) star_exit, 1, IM(conString)); */
}
