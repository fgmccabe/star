//
// Created by Francis McCabe on 7/9/20.
//
#include <config.h>
#include <assert.h>
#include "lowerP.h"
#include "stackP.h"
#include "labelsP.h"
#include "globals.h"
#include "constantsP.h"
#include "jitP.h"
#include "formioP.h"
#include "debug.h"
#include "errorCodes.h"

/* Lower Star VM code to Arm64 code */

/* Register allocation for arm64:
 *
 * X0-X7 = integer parameters
 * X0 = return register
 * X8-X15 = caller saved scratch registers
 * X16-X17 = intra procedure call scratch registers
 * X18 = platform register
 * X25 = Constants vector
 * AG = X26 = args pointer
 * STK = X27 = current stack structure pointer
 * SSP = X28 = star stack pointer
 * FP = X29 = frame pointer
 * LR = X30 = link register
 * SP = X31 = system stack pointer
 */

static retCode stackCheck(jitCompPo jit, methodPo mtd);

static int32 pointerSize = sizeof(integer);

static int32 argOffset(int32 argNo) {
  return argNo * pointerSize;
}

static retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun);

static retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun);

static retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun);

static retCode bail(jitCompPo jit, char *msg, ...);

static retCode loadStackIntoArgRegisters(jitCompPo jit, armReg startRg, uint32 arity);

static retCode getIntVal(jitCompPo jit, armReg rg);
static retCode mkIntVal(jitCompPo jit, armReg rg);
static retCode getFltVal(jitCompPo jit, armReg rg);
static retCode mkFltVal(jitCompPo jit, armReg rg);

#define SSP (X28)
#define AG  (X26)
#define STK (X27)
#define CO (X25)

static int32 fpArgs = OffsetOf(StackFrame, args);
static int32 fpProg = OffsetOf(StackFrame, prog);
static int32 fpLink = OffsetOf(StackFrame, link);

static retCode jitError(jitCompPo jit, char *msg, ...);

static jitBlockPo breakBlock(jitBlockPo block, int32 tgt);
static codeLblPo breakLabel(jitBlockPo block);
static codeLblPo loopLabel(jitBlockPo block);
retCode breakOutEq(jitBlockPo block, int32 tgt);
retCode breakOutNe(jitBlockPo block, int32 tgt);
retCode breakOut(jitBlockPo block, int32 tgt, logical keepTop);

void stashRegisters(jitCompPo jit);
void unstashRegisters(jitCompPo jit);

retCode invokeJitMethod(methodPo mtd, heapPo H, stackPo stk) {
  jittedCode code = jitCode(mtd);

  framePo fp = stk->fp + 1;

  // Preload a new frame
  stk->fp = fp;
  fp->prog = stk->prog;
  fp->args = stk->args;

  asm( "stp x29, x30, [sp, #-16]!\n"
       "stp x27, x28, [sp, #-16]!\n"
       "stp x25, x26, [sp, #-16]!\n"
       "ldr x27, %[stk]\n"
       "ldr x28, %[ssp]\n"
       "ldr x26, %[ag]\n"
       "ldr x25, %[constants]\n"
       "ldr x16, %[code]\n"
       "ldr x29, %[fp]\n"
       "blr x16\n"
       "str x26, %[ag]\n"
       "str x27, %[stk]\n"
       "str x29, %[fp]\n"
       "ldp x25, x26, [sp], #16\n"
       "ldp x27, x28, [sp], #16\n"
       "ldp x29, x30, [sp], #16\n"
    : [stk] "=m"(stk), [ssp] "=m"(stk->sp), [ag] "=m"(stk->args), [code] "=m"(code), [fp] "=m"(stk->fp),
  [fplink] "=m"(fp->link), [constants] "=m"(constAnts)
  :
  : "x0", "x1", "x2", "x3", "x25", "x26", "x27", "x28", "x30", "cc", "memory");

  return Ok;
}

static armReg popStkOp(jitBlockPo block, armReg tgt) {
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);

  ldr(tgt, PSX(SSP, pointerSize));
  return tgt;
}

static armReg topStkOp(jitBlockPo block) {
  jitCompPo jit = block->jit;
  armReg tgt = findFreeReg(jit);
  assemCtxPo ctx = assemCtx(jit);
  ldr(tgt, RG(SSP));

  return tgt;
}

static void pushStkOp(jitBlockPo block, armReg src) {
  assemCtxPo ctx = assemCtx(block->jit);
  str(src, PRX(SSP, -pointerSize));
}

static armReg loadConstant(jitCompPo jit, int32 key, armReg tgt) {
  assemCtxPo ctx = assemCtx(jit);
  termPo lit = getConstant(key);

  if (isSmall(lit))
    mov(tgt, IM((integer) lit));
  else {
    ldr(tgt, OF(CO, key * pointerSize));
  }

  return tgt;
}

static retCode jitBlock(jitBlockPo block, int32 from, int32 endPc) {
  retCode ret = Ok;
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(jit);
  insPo code = block->code;

  for (int32 pc = from; ret == Ok && pc < endPc;) {
    switch (code[pc].op) {
      case Halt: {
        // Stop execution
        integer errCode = code[pc].fst;
        ret = callIntrinsic(ctx, (runtimeFn) star_exit, 1, IM(errCode));
        pc++;
        continue;
      }
      case Nop: // No operation
        pc++;
        continue;
      case Abort: // abort with message
        pc++;
        return Error;
      case Call: {
        // Call <prog>
        int32 key = code[pc].fst;
        int arity = codeArity(labelCode(C_LBL(getConstant(key))));

        loadConstant(jit, key, X16);
        // pick up the pointer to the method
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bail(jit, "Function %T not defined", getConstant(key));

        setLabel(ctx, haveMtd);

        add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
        str(AG, OF(FP, fpArgs));
        mov(X0, IM((integer) jit->mtd));
        str(X0, OF(FP, fpProg)); // We know what program we are executing
        str(X16, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit)));
        blr(X16);
        pc++;
        continue;
      }
      case XCall: {
        // Call <prog>, with catch
        labelPo nProg = C_LBL(getConstant(code[pc].fst));

        // pick up the pointer to the method
        mov(X16, IM((integer) nProg));
        ldr(X17, OF(X16, OffsetOf(LblRecord, mtd)));

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X17, haveMtd);

        bail(jit, "Function %T not defined", nProg);

        setLabel(ctx, haveMtd);

        add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
        str(AG, OF(FP, fpArgs));
        ldr(X0, OF(STK, OffsetOf(StackRecord, prog)));
        str(X0, OF(FP, fpProg)); // Copy from stack
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit)));

        codeLblPo returnPc = newLabel(ctx);
        adr(LR, returnPc);

        br(X16);
        codeLblPo brking = currentPcLabel(ctx);
        ret = breakOut(block, pc + code[pc].alt + 1, True);
        b(brking); // step back to the break out code
        setLabel(ctx, returnPc);
        pc++;
        continue;
      }

      case OCall: { // OCall
        int32 arity = code[pc].fst;

        popStkOp(block, X16);          // Pick up the closure
        ldr(X17, OF(X16, 0));                // Pick up the label
        // pick up the pointer to the method
        ldr(X0, OF(X17, OffsetOf(LblRecord, mtd)));

        ldr(X16, OF(X16, pointerSize));    // Pick up the free term
        pushStkOp(block, X16);        // The free term isthe first argument

        codeLblPo haveMtd = newLabel(ctx);
        cbnz(X0, haveMtd);

        bail(jit, "Function not defined");

        setLabel(ctx, haveMtd);

        add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
        str(AG, OF(FP, fpArgs));
        mov(X0, IM((integer) jit->mtd));
        str(X0, OF(FP, fpProg)); // We know what program we are executing
        str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program

        // Pick up the jit code itself
        ldr(X16, OF(X17, OffsetOf(MethodRec, jit)));
        blr(X16);
        pc++;
        return Error;
      }
      case Escape: {
        // call C escape
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        loadStackIntoArgRegisters(jit, X1, arity);
        loadCGlobal(ctx, X0, (void *) &globalHeap);
        stashRegisters(jit);
        stp(X0,X0,PRX(SP,-2*pointerSize)); // leave room for the fat return value
        callIntrinsic(ctx, (runtimeFn) escapeFun(esc), arity + 1, RG(X0), RG(X1), RG(X2), RG(X3), RG(X4), RG(X5),
                      RG(X6), RG(X7), RG(X8));
        ldp(X0,X1,PSX(SP,2*pointerSize));
        unstashRegisters(jit);
        // X0 is the return code - which we ignore for normal escapes
        // X1 is the return value
        pushStkOp(block, X1);
        pc++;
        continue;
      }
      case XEscape: {
        // call C escape, with possible exception return
        int32 escNo = code[pc].fst;
        escapePo esc = getEscape(escNo);
        int32 arity = escapeArity(esc);

        loadStackIntoArgRegisters(jit, X1, arity);
        loadCGlobal(ctx, X0, (void *) &globalHeap);
        stashRegisters(jit);
        callIntrinsic(ctx, (runtimeFn) escapeFun(esc), arity + 1, RG(X0), RG(X1), RG(X2), RG(X3), RG(X4), RG(X5),
                      RG(X6), RG(X7), RG(X8));
        unstashRegisters(jit);
        // X0 is the return code - which we ignore for normal escapes
        codeLblPo next = newLabel(ctx);
        pushStkOp(block, X1);        // X1 is the return value
        tstw(X0, RG(X0));
        beq(next);
        ret = breakOut(block, pc + code[pc].alt + 1, False);

        setLabel(ctx, next);
        pc++;
        continue;
      }
      case XOCall:
      case TCall: // TCall <prog>
      case TOCall: // TOCall
        return Error;
      case Entry: {
        // locals definition
        int32 locals = code[pc].fst;
        assert(locals >= 0);

        str(LR, OF(FP, fpLink));

        tryRet(stackCheck(jit, jit->mtd));

        if (locals > 0) {
          armReg vd = findFreeReg(jit);
          mov(vd, IM((integer) voidEnum));
          if (locals < 8) {
            for (int32 ix = 0; ix < locals; ix++) {
              pushStkOp(block, vd);
            }
          } else {
            // Build a loop
            armReg cx = findFreeReg(jit);
            mov(cx, IM(locals));
            codeLblPo start = currentPcLabel(ctx);
            pushStkOp(block, vd);
            sub(cx, cx, IM(1));
            bne(start);
            releaseReg(jit, cx);
          }
          releaseReg(jit, vd);
        }
        mov(AG, RG(SSP));
        pc++;
        continue;
      }
      case Ret: {
        // return
        armReg vl = popStkOp(block, findFreeReg(jit));

        // Pick up the caller program
        ldr(X16, OF(FP, fpProg));
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Adjust Star stack and args register
        add(SSP, AG, IM(codeArity(jit->mtd) * pointerSize));
        ldr(AG, OF(FP, fpArgs));
        // Pick up return address
        ldr(X16, OF(FP, fpLink));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        // Put return value on stack
        pushStkOp(block, vl);
        releaseReg(jit, vl);
        eor(X0, X0, RG(X0));
        br(X16);
        pc++;
        continue;
      }
      case XRet: {
        // exception return
        armReg vl = popStkOp(block, findFreeReg(jit));

        // Pick up the caller program
        ldr(X16, OF(FP, fpProg));
        str(X16, OF(STK, OffsetOf(StackRecord, prog)));

        // Adjust Star stack and args register
        add(SSP, AG, IM(codeArity(jit->mtd) * pointerSize));
        ldr(AG, OF(FP, fpArgs));
        // Pick up return address
        ldr(X16, OF(FP, fpLink));
        // Drop frame
        sub(FP, FP, IM(sizeof(StackFrame)));
        // Put return value on stack
        pushStkOp(block, vl);
        releaseReg(jit, vl);

        sub(X16, X16, IM(pointerSize)); // Step back one instruction to get to the break
        br(X16);
        pc++;
        continue;
      }
      case Block: {
        // block of instructions
        int32 blockLen = code[pc].alt;
        int32 blockHeight = code[pc].fst;
        codeLblPo brkLbl = newLabel(ctx);
        pc++;

        JitBlock subBlock = {
          .jit = block->jit,
          .code = code,
          .valStk = block->valStk,
          .startPc = pc - 1,
          .breakLbl = brkLbl,
          .loopLbl = currentPcLabel(ctx),
          .parent = block,
        };

        ret = jitBlock(&subBlock, pc, pc + blockLen);

        pc += blockLen;
        setLabel(ctx, brkLbl);
        add(SSP, AG, IM((lclCount(jit->mtd) + blockHeight) * pointerSize));
        continue;
      }
      case Break: {
        // leave block
        ret = breakOut(block, pc + code[pc].alt + 1, False);
        pc++;
        continue;
      }
      case Result: {
        // return value out of block
        ret = breakOut(block, pc + code[pc].alt + 1, True);
        pc++;
        continue;
      }
      case Loop: {
        // jump back to start of block
        jitBlockPo tgtBlock = breakBlock(block, pc + code[pc].alt + 1);
        codeLblPo tgt = loopLabel(tgtBlock);
        assert(tgt != Null);
        add(SSP, AG, IM((lclCount(jit->mtd) + code[tgtBlock->startPc].fst) * pointerSize));
        b(tgt);
        pc++;
        continue;
      }
      case Drop: {
        // drop top of stack
        add(SSP, SSP, IM(pointerSize));
        pc++;
        continue;
      }
      case Dup: {
        // duplicate top of stack
        armReg tgt = topStkOp(block);
        pushStkOp(block, tgt);
        releaseReg(jit, tgt);
        pc++;
        continue;
      }
      case Rot: {
        // Pull up nth element of stack
        int32 rotationHeight = code[pc].fst;
        armReg swp = findFreeReg(jit);
        armReg src = findFreeReg(jit);

        ldr(swp, OF(SSP, jit->currSPOffset));

        for (int32 ix = 1; ix <= rotationHeight; ix++) {
          ldr(src, OF(SSP, jit->currSPOffset + (ix * sizeof(integer))));
          str(src, OF(SSP, jit->currSPOffset + ((ix - 1) * sizeof(integer))));
        }
        str(swp, OF(SSP, jit->currSPOffset + rotationHeight * sizeof(integer)));
        releaseReg(jit, swp);
        releaseReg(jit, src);
        pc++;
        continue;
      }
      case Rst: {
        // reset stack height to a fixed height
        int32 stkHeight = code[pc].fst;
        int32 lx = lclCount(jit->mtd);
        int32 heightOffset = (lx + stkHeight) * pointerSize;

        sub(SSP, AG, IM(heightOffset));
        pc++;
        continue;
      }
      case Pick: {
        // adjust stack to n depth, using top k elements
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
      case Fiber: // Create new fiber
      case Suspend: // suspend fiber
      case Resume: // resume fiber
      case Retire: // retire a fiber
      case Underflow: // underflow from current stack
      case LdV: {
        // Place a void value on stack
        armReg vd = findFreeReg(jit);
        mov(vd, IM((integer) voidEnum));
        pushStkOp(block, vd);
        releaseReg(jit, vd);
        pc++;
        continue;
      }
      case LdC: {
        // load literal from constant pool
        int32 key = code[pc].fst;
        armReg cn = loadConstant(jit, key, findFreeReg(jit));

        pushStkOp(block, cn);
        releaseReg(jit, cn);

        pc++;
        continue;
      }
      case LdA: {
        // load stack from args[xx]
        int32 argNo = code[pc].fst;
        armReg rg = findFreeReg(jit);
        ldr(rg, OF(AG, argOffset(argNo)));
        pushStkOp(block, rg);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case LdL: {
        // load stack from local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg rg = findFreeReg(jit);
        ldr(rg, OF(AG, offset));
        pushStkOp(block, rg);
        releaseReg(jit, rg);
        pc++;
        continue;
      }
      case StL: {
        // store tos to local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vl = popStkOp(block, findFreeReg(jit));
        str(vl, OF(AG, offset));
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case StV: {
        // clear a local to void
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vd = findFreeReg(jit);
        mov(vd, IM((integer) voidEnum));
        str(vd, OF(AG, offset));
        releaseReg(jit, vd);
        pc++;
        continue;
      }
      case TL: {
        // copy tos to local[xx]
        int32 lclNo = code[pc].fst;
        int32 offset = -lclNo * pointerSize;
        armReg vl = topStkOp(block);
        str(vl, OF(AG, offset));
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case LdG: {
        // load a global variable
        verifyJitCtx(jit, 1, 0);
        int32 litNo = code[pc].fst;
        vOperand lclOp = {.loc = global, .ix = litNo};
        pc++;
        continue;
      }
      case StG: // store into a global variable
        return Error;
      case TG: // copy into a global variable
        return Error;
      case Sav: // create a single assignment variable
        return Error;
      case LdSav: // derefence a sav, break if not set
        return Error;
      case TstSav: // test a sav, return a logical
        return Error;
      case StSav: // store a value into a single assignment variable
        return Error;
      case TSav: // update single assignment variable leave value on stack
        return Error;
      case Cell: // create R/W cell
      case Get: // access a R/W cell
      case Assign: // assign to a R/W cell
      case CLbl: // T,Lbl --> test for a data term, break if not lbl
        return Error;
      case CInt:
      case CChar:
      case CFlt: {
        armReg st = popStkOp(block, findFreeReg(jit));
        integer lit = (integer) getConstant(code[pc].fst);
        if (is12bit(lit))
          cmp(st, IM(lit));
        else {
          armReg lt = findFreeReg(jit);
          mov(lt, IM(lit));
          cmp(st, RG(lt));
          releaseReg(jit, lt);
        }
        releaseReg(jit, st);
        ret = breakOutNe(block, pc + code[pc].alt + 1);
        pc++;
        continue;
      }
      case CLit: {
        // T,lit --> test for a literal value, break if not
        int32 key = code[pc].fst;

        tryRet(reserveReg(jit, X0));
        tryRet(reserveReg(jit, X1));
        loadConstant(jit, key, X0);
        popStkOp(block, X1);
        invokeCFunc2(jit, (Cfunc2) sameTerm);
        tst(X0, RG(X0));
        ret = breakOutEq(block, pc + code[pc].alt + 1);
        pc++;
        releaseReg(jit, X0);
        releaseReg(jit, X1);
        continue;
      }

      case Nth: {
        // T --> el, pick up the nth element
        armReg vl = popStkOp(block, findFreeReg(jit));
        ldr(vl, OF(vl, (code[pc].fst + 1) * pointerSize));
        pushStkOp(block, vl);
        releaseReg(jit, vl);
        pc++;
        continue;
      }
      case StNth: {
        // T el --> store in nth element
        armReg trm = popStkOp(block, findFreeReg(jit));
        armReg vl = popStkOp(block, findFreeReg(jit));
        str(vl, OF(trm, (code[pc].fst + 1) * pointerSize));

        releaseReg(jit, vl);
        releaseReg(jit, trm);
        pc++;
        continue;
      }
      case If: {
        // break if true
        armReg vl = popStkOp(block, findFreeReg(jit));
        armReg tr = findFreeReg(jit);
        mov(tr, IM((integer) trueEnum));
        cmp(vl, RG(tr));
        releaseReg(jit, tr);
        releaseReg(jit, vl);
        ret = breakOutNe(block, pc + code[pc].alt + 1);
        pc++;
        continue;
      }
      case IfNot: {
        // break if false
        armReg vl = popStkOp(block, findFreeReg(jit));
        armReg tr = findFreeReg(jit);
        mov(tr, IM((integer) trueEnum));
        cmp(vl, RG(tr));
        releaseReg(jit, tr);
        releaseReg(jit, vl);
        ret = breakOutEq(block, pc + code[pc].alt + 1);
        pc++;
        continue;
      }
      case ICase: {
        armReg gr = popStkOp(block, findFreeReg(jit));
        getIntVal(jit, gr);
        and(gr, gr, IM(LARGE_INT61));
        armReg divisor = findFreeReg(jit);
        mov(divisor, IM(code[pc].fst));
        armReg quotient = findFreeReg(jit);
        udiv(quotient, gr, divisor);
        msub(gr, divisor, quotient, gr);
        releaseReg(jit, divisor);
        armReg tgt = findFreeReg(jit);
        codeLblPo jmpTbl = newLabel(ctx);
        adr(tgt, jmpTbl);
        add(tgt, tgt, LS(gr, 2));
        br(tgt);
        releaseReg(jit, tgt);
        releaseReg(jit, quotient);
        releaseReg(jit, gr);
        setLabel(ctx, jmpTbl);
        pc++;
        continue;
      }
      case Case: {
        // T --> T, case <Max>
        if (reserveReg(jit, X0) == Ok) {
          popStkOp(block, X0);
          callIntrinsic(ctx, (runtimeFn) hashTerm, 1, RG(X0));
          armReg divisor = findFreeReg(jit);
          mov(divisor, IM(code[pc].fst));
          armReg quotient = findFreeReg(jit);
          udiv(quotient, X0, divisor);
          msub(X0, divisor, quotient, X0);
          releaseReg(jit, divisor);
          armReg tgt = findFreeReg(jit);
          codeLblPo jmpTbl = newLabel(ctx);
          adr(tgt, jmpTbl);
          add(tgt, tgt, LS(X0, 2));
          br(tgt);
          releaseReg(jit, tgt);
          releaseReg(jit, quotient);
          releaseReg(jit, X0);
          setLabel(ctx, jmpTbl);
          pc++;
          continue;
        } else {
          return jitError(jit, "cannot reserve R0");
        }
      }
      case IxCase: {
        // check and jump on index
        armReg tgt = popStkOp(block, findFreeReg(jit));
        armReg ix = findFreeReg(jit);
        ldr(ix, OF(tgt, 0)); // Pick up the label
        ldr(ix, OF(ix, OffsetOf(LblRecord, index)));
        // Make sure that it is less than max
        armReg divisor = findFreeReg(jit);
        mov(divisor, IM(code[pc].fst));
        armReg quotient = findFreeReg(jit);
        udiv(quotient, ix, divisor);
        msub(ix, divisor, quotient, ix);

        codeLblPo jmpTbl = newLabel(ctx);
        adr(tgt, jmpTbl);
        add(tgt, tgt, LS(ix, 2));
        br(tgt);
        releaseReg(jit, tgt);
        releaseReg(jit, quotient);
        releaseReg(jit, ix);
        setLabel(ctx, jmpTbl);
        pc++;
      }
      case IAdd: {
        // L R --> L+R
        armReg a1 = popStkOp(block, findFreeReg(jit));
        armReg a2 = popStkOp(block, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        add(a1, a2, RG(a1));

        mkIntVal(jit, a1);

        pushStkOp(block, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case ISub: {
        // L R --> L-R
        armReg a1 = popStkOp(block, findFreeReg(jit));
        armReg a2 = popStkOp(block, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        sub(a1, a1, RG(a2));

        mkIntVal(jit, a1);
        pushStkOp(block, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case IMul: {
        // L R --> L*R
        armReg a1 = popStkOp(block, findFreeReg(jit));
        armReg a2 = popStkOp(block, findFreeReg(jit));

        getIntVal(jit, a1);
        getIntVal(jit, a2);

        mul(a1, a2, a1);

        mkIntVal(jit, a1);

        pushStkOp(block, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);

        pc++;
        continue;
      }
      case IDiv: {
        // L R --> L/R
        armReg a1 = popStkOp(block, findFreeReg(jit));
        armReg a2 = popStkOp(block, findFreeReg(jit));
        getIntVal(jit, a1);
        getIntVal(jit, a2);

        codeLblPo skip = newLabel(ctx);
        tst(a2, IM(0));
        bne(skip);
        int32 divZeroKey = defineConstantLiteral(divZero);

        loadConstant(jit, divZeroKey, a2);
        pushStkOp(block, a2);
        jitBlockPo tgtBlock = breakBlock(block, pc + code[pc].alt + 1);
        codeLblPo lbl = breakLabel(tgtBlock);
        if (lbl != Null) {
          add(SSP, AG, IM(-(lclCount(jit->mtd) + code[tgtBlock->startPc].fst - 1) * pointerSize));
          b(lbl);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        setLabel(ctx, skip);
        sdiv(a1, a1, a2);
        mkIntVal(jit, a1);
        pushStkOp(block, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case IMod: // L R --> L%R
      case IAbs: // L --> abs(L)
      case IEq: // L R --> L==R
      case ILt: // L R --> L<R
      case IGe: // L R --> L>=R
      case ICmp: // L R --> break if not same integer
      case CEq: // L R --> L==R
      case CLt: // L R --> L<R
      case CGe: // L R --> L>=R
      case CCmp: // L R --> break if not same character
      case BAnd: // L R --> L&R
      case BOr: // L R --> L|R
      case BXor: // L R --> L^R
      case BLsl: // L R --> L<<R
      case BLsr: // L R --> L>>R
      case BAsr: // L R --> L>>>R
      case BNot: // L --> ~L
      case FAdd: // L R --> L+R
      case FSub: // L R --> L-R
      case FMul: // L R --> L*R
      case FDiv: {
        // L R --> L/R
        armReg a1 = popStkOp(block, findFreeReg(jit));
        armReg a2 = popStkOp(block, findFreeReg(jit));
        getFltVal(jit, a1);
        getFltVal(jit, a2);

        codeLblPo skip = newLabel(ctx);
        tst(a2, IM(0));
        bne(skip);
        int32 divZeroKey = defineConstantLiteral(divZero);

        jitBlockPo tgtBlock = breakBlock(block, pc + code[pc].alt + 1);

        if (tgtBlock != Null) {
          add(SSP, AG, IM(-(lclCount(jit->mtd) + code[tgtBlock->startPc].fst - 1) * pointerSize));
          loadConstant(jit, divZeroKey, a2);
          pushStkOp(block, a2);

          codeLblPo lbl = breakLabel(tgtBlock);
          if (lbl != Null) {
            b(lbl);
          } else
            return jitError(jit, "cannot find target label for %d", tgtBlock);
        } else
          return jitError(jit, "cannot find target label for %d", tgtBlock);

        setLabel(ctx, skip);
        fmov(FP(F0), RG(a1));
        fmov(FP(F1), RG(a2));
        fdiv(F0, F0, F1);
        fmov(RG(a1), FP(F0));
        mkFltVal(jit, a1);
        pushStkOp(block, a1);

        releaseReg(jit, a1);
        releaseReg(jit, a2);
        pc++;
        continue;
      }
      case FMod: // L R --> L%R
      case FAbs: // L --> abs(L)
      case FEq: // L R e --> L==R
      case FLt: // L R --> L<R
      case FGe: // L R --> L>=R
      case FCmp: // L R --> branch if not same floating point
      case Alloc: // new structure, elements from stack
        return Error;
      case Closure: // allocate a closure
        return Error;
      case Cmp: // t1 t2 --> , branch to offset if not same literal
        return Error;
      case Frame: {
        // frame instruction
        pc++;
        continue;
      }
      case dBug: // debugging prefix
      case maxOpCode:
      case illegalOp:
        return Error;
    }
  }

  return ret;
}

retCode jitInstructions(jitCompPo jit, methodPo mtd, char *errMsg, integer msgLen) {
#ifdef TRACEJIT
  if (traceJit > noTracing) {
    showMethodCode(logFile, "Jit method %L\n", mtd);
  }
#endif
  ValueStack valueStack = {.stackHeight = 0};
  JitBlock block = {
    .jit = jit,
    .code = entryPoint(mtd),
    .startPc = 0, .valStk = &valueStack, .breakLbl = Null, .loopLbl = Null, .parent = Null
  };

  return jitBlock(&block, 0, codeSize(mtd));
}

retCode invokeCFunc1(jitCompPo jit, Cfunc1 fun) {
  return callIntrinsic(assemCtx(jit), (runtimeFn) fun, 1, RG(X0));
}

retCode invokeCFunc2(jitCompPo jit, Cfunc2 fun) {
  return callIntrinsic(assemCtx(jit), (runtimeFn) fun, 2, RG(X0), RG(X1));
}

retCode invokeCFunc3(jitCompPo jit, Cfunc3 fun) {
  return callIntrinsic(assemCtx(jit), (runtimeFn) fun, 3, RG(X0), RG(X1), RG(X2));
}

retCode loadStackIntoArgRegisters(jitCompPo jit, armReg startRg, uint32 arity) {
  assemCtxPo ctx = assemCtx(jit);
  assert(arity < 9);

  for (uint32 ix = 0; ix < arity; ix++) {
    ldr((armReg) (startRg + ix), PSX(SSP, pointerSize));
  }
  return Ok;
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
  ldr(dst, OF(glbHeap, currOff)); // pick up current heap top
  add(scratch, dst, amnt);
  ldr(limit, OF(glbHeap, limitOff)); // check against limit
  cmp(limit, RG(scratch));
  bgt(okLbl);

  // Invoke out of line allocator


  b(endLbl);

  setLabel(ctx, okLbl);
  str(scratch, OF(glbHeap, currOff)); // record new heap top
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

retCode getFltVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  asr(rg, rg, IM(2));
  return Ok;
}

retCode mkFltVal(jitCompPo jit, armReg rg) {
  assemCtxPo ctx = assemCtx(jit);
  lsl(rg, rg, IM(2));
  orr(rg, rg, IM(fltTg));
  return Ok;
}

retCode breakOutEq(jitBlockPo block, int32 tgt) {
  jitBlockPo tgtBlock = breakBlock(block, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);

  if (lbl != Null) {
    beq(lbl);
    return Ok;
  } else
    return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOutNe(jitBlockPo block, int32 tgt) {
  jitBlockPo tgtBlock = breakBlock(block, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);

  if (lbl != Null) {
    bne(lbl);
    return Ok;
  } else
    return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOut(jitBlockPo block, int32 tgt, logical keepTop) {
  jitBlockPo tgtBlock = breakBlock(block, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);
  insPo code = block->code;

  if (lbl != Null) {
    if (keepTop) {
      armReg tp = findFreeReg(jit);
      popStkOp(block, tp);
      add(SSP, AG, IM((lclCount(jit->mtd) + code[tgtBlock->startPc].fst - 1) * pointerSize));
      pushStkOp(block, tp);
      releaseReg(jit, tp);
    }
    b(lbl);
    return Ok;
  } else
    return jitError(jit, "cannot find target label for %d", tgt);
}

jitBlockPo breakBlock(jitBlockPo block, int32 tgt) {
  while (block != Null) {
    if (block->startPc == tgt) {
      assert(block->code[block->startPc].op == Block);
      return block;
    } else
      block = block->parent;
  }
  return Null;
}

codeLblPo breakLabel(jitBlockPo block) {
  return block->breakLbl;
}

codeLblPo loopLabel(jitBlockPo block) {
  return block->loopLbl;
}

retCode jitError(jitCompPo jit, char *msg, ...) {
  char buff[MAXLINE];
  strBufferPo f = fixedStringBuffer(buff, NumberOf(buff));

  va_list args; /* access the generic arguments */
  va_start(args, msg); /* start the variable argument sequence */

  __voutMsg(O_IO(f), msg, args); /* Display into the string buffer */

  va_end(args);
  outByte(O_IO(f), 0); /* Terminate the string */

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

retCode stackCheck(jitCompPo jit, methodPo mtd) {
  assemCtxPo ctx = assemCtx(jit);
  codeLblPo okLbl = newLabel(ctx);
  int32 delta = (int32) (stackDelta(mtd) + FrameCellCount) * pointerSize;

  if (is16bit(delta))
    sub(X16, SSP, IM(delta));
  else {
    mov(X16, IM(delta));
    sub(X16, SSP, RG(X16));
  }
  cmp(X16, RG(FP));
  bhi(okLbl);

  str(AG, OF(STK, OffsetOf(StackRecord, args)));
  str(SSP, OF(STK, OffsetOf(StackRecord, sp)));
  str(FP, OF(STK, OffsetOf(StackRecord, fp)));
  mov(X0, IM((integer) mtd));
  str(X0, OF(STK, OffsetOf(StackRecord, prog)));

  stp(CO, X0, PRX(SP, 2 * pointerSize));

  tryRet(callIntrinsic(ctx, (runtimeFn) handleStackOverflow, 3, RG(STK), IM(delta), IM((integer) codeArity(mtd))));

  mov(STK, RG(X0));
  ldp(CO, X0, PSX(SP, 2 * pointerSize));

  ldr(FP, OF(STK, OffsetOf(StackRecord, fp)));
  ldr(SSP, OF(STK, OffsetOf(StackRecord, sp)));
  ldr(AG, OF(STK, OffsetOf(StackRecord, args)));

  setLabel(ctx, okLbl);
  return Ok;
}

void stashRegisters(jitCompPo jit){
  assemCtxPo ctx = assemCtx(jit);
  stp(STK,CO,PRX(SP,-2*pointerSize));
  str(AG,OF(X27, OffsetOf(StackRecord,args)));
  str(SSP,OF(X27, OffsetOf(StackRecord,sp)));
  str(FP,OF(X27,OffsetOf(StackRecord,fp)));
}

void unstashRegisters(jitCompPo jit){
  assemCtxPo ctx = assemCtx(jit);
  ldp(STK,CO,PRX(SP,2*pointerSize));
  ldr(AG,OF(X27, OffsetOf(StackRecord,args)));
  ldr(SSP,OF(X27, OffsetOf(StackRecord,sp)));
  ldr(FP,OF(X27,OffsetOf(StackRecord,fp)));
}
