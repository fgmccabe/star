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
  jitBlockPo tgtBlock = breakBlock(block, code, tgt, Block);
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
  jitBlockPo tgtBlock = breakBlock(block, code, tgt, Block);
  codeLblPo lbl = breakLabel(tgtBlock);
  jitCompPo jit = block->jit;

  if (lbl != Null) {
    assemCtxPo ctx = assemCtx(jit);
    bne(lbl);
    return Ok;
  }
  return jitError(jit, "cannot find target label for %d", tgt);
}

retCode breakOut(assemCtxPo ctx, jitBlockPo tgtBlock) {
  codeLblPo lbl = breakLabel(tgtBlock);
  b(lbl);
  return Ok;
}

codeLblPo getABreakLbl(jitBlockPo block, const int32 pc) {
  jitBlockPo tgtBlock = block;
  while (tgtBlock != Null && tgtBlock->endPc < pc)
    tgtBlock = tgtBlock->parent;
  if (tgtBlock != Null)
    return tgtBlock->breakLbl;
  else
    return Null;
}

jitBlockPo breakBlock(jitBlockPo block, insPo code, int32 tgt, OpCode blockType) {
  while (block != Null) {
    if (block->startPc == tgt) {
      assert(code[block->startPc].op == blockType);
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
