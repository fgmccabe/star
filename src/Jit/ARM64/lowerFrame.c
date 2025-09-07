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
      int32 tgtOff = tgtBlock->exitHeight;
      if (tgtOff != block->stack.vTop) {
        // already at the right height?
        LocalEntry top = *stackSlot(&block->stack,0);
        block->stack.vTop = tgtOff;
        block->stack.local[tgtOff - 1] = top;
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
    }
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
