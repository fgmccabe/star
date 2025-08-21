//
// Created by Francis McCabe on 8/20/25.
//

#include <config.h>

#include "cellP.h"
#include "lowerP.h"
#include "stackP.h"
#include "jitP.h"
#include "debug.h"
#include "engineP.h"

void pshFrame(jitCompPo jit, assemCtxPo ctx, armReg mtdRg) {
  add(FP, FP, IM(sizeof(StackFrame))); // Bump the current frame
  str(AG, OF(FP, OffsetOf(StackFrame, args)));
  sub(AG, AG, IM(jitTrueStackDepth(jit)*pointerSize));
  armReg tmp = findFreeReg(jit);
  ldr(tmp, OF(STK, OffsetOf(StackRecord, prog)));
  str(tmp, OF(FP, OffsetOf(StackFrame, prog))); // We know what program we are executing
  str(mtdRg, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  releaseReg(jit, tmp);
}

void overrideFrame(jitCompPo jit, assemCtxPo ctx, int arity) {
  armReg src = findFreeReg(jit);
  armReg tgt = findFreeReg(jit);
  armReg tmp = findFreeReg(jit); // Overwrite existing arguments and locals
  sub(src, AG, IM((jitTrueStackDepth(jit)-arity) * pointerSize));
  add(tgt, AG, IM(argCount(jit->mtd) * pointerSize));
  if (arity < 8) {
    for (int ix = 0; ix < arity; ix++) {
      ldr(tmp, PRX(src, -pointerSize));
      str(tmp, PRX(tgt, -pointerSize));
    }
  } else {
    // Build a loop
    armReg cx = findFreeReg(jit);
    mov(cx, IM(arity));
    codeLblPo start = here();

    ldr(tmp, PRX(src, -pointerSize));
    str(tmp, PRX(tgt, -pointerSize));

    subs(cx, cx, IM(1));
    bne(start);
    releaseReg(jit, cx);
  }
  // Update current frame
  str(X17, OF(STK, OffsetOf(StackRecord, prog))); // Set new current program
  mov(AG, RG(tgt));

  releaseReg(jit, tmp);
  releaseReg(jit, src);
  releaseReg(jit, tgt);
}


retCode breakOutEq(jitBlockPo block, insPo code, int32 tgt) {
  jitBlockPo tgtBlock = breakBlock(block, code, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);

  if (lbl != Null) {
    beq(lbl);
    return Ok;
  } else
    return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOutNe(jitBlockPo block, insPo code, int32 tgt) {
  jitBlockPo tgtBlock = breakBlock(block, code, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;

  if (lbl != Null) {
    assemCtxPo ctx = assemCtx(jit);
    bne(lbl);
    return Ok;
  }
  return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOut(jitBlockPo block, insPo code, int32 tgt, logical keepTop) {
  jitBlockPo tgtBlock = breakBlock(block, code, tgt);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;
  assemCtxPo ctx = assemCtx(block->jit);

  if (lbl != Null) {
    if (keepTop) {
      int32 tgtOff = code[tgtBlock->startPc].fst;
      if (tgtOff != jit->stackDepth) {
        armReg tp = topStkOp(jit);
        storeStack(jit, tp, tgtOff);
        releaseReg(jit, tp);
      }
    }

    b(lbl);
    return Ok;
  }
  return jitError(jit, "cannot find target label for %d", tgt);
}

jitBlockPo breakBlock(jitBlockPo block, insPo code, int32 tgt) {
  while (block != Null) {
    if (block->startPc == tgt) {
      assert(code[block->startPc].op == Block);
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
