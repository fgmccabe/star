/*
 * Intel x64 assembler
 * Intended to be called from C code
 */

#include <assert.h>
#include <utils.h>

#include "x86_64P.h"
#include "jitP.h"
#include "jit.h"

codeLblPo preamble(assemCtxPo ctx, int32 lclCount) {
  codeLblPo entry = defineLabel(ctx, ctx->pc);
  push(RG(RBP));
  mov(RG(RBP), RG(RSP));
  sub(RG(RSP), IM(lclCount));
  return entry;
}

retCode postamble(assemCtxPo ctx) {
  mov(RG(RSP), RG(RBP));
  pop(RG(RBP));
  rtn();
  return Ok;
}

void clearCodeCtxMaps(assemCtxPo ctx) {
//  ctx->usedRegs = 0;
}

logical sameFlexOp(FlexOp a, FlexOp b) {
  if (a.mode != b.mode || a.size != b.size)
    return False;
  switch (a.mode) {
    case Reg:
      return a.op.reg == b.op.reg;
    case Fp:
      return a.op.fpReg == b.op.fpReg;
    case Immediate:
      return a.op.imm == b.op.imm;
    case Based:
      return a.op.based.base == b.op.based.base && a.op.based.disp == b.op.based.disp;
    case Indexed:
      return a.op.indexed.base == b.op.indexed.base &&
             a.op.indexed.index == b.op.indexed.index &&
             a.op.indexed.scale == b.op.indexed.scale &&
             a.op.indexed.disp == b.op.indexed.disp;
    case Labeled:
      return a.op.lbl == b.op.lbl;
    default:
      return False;
  }
}
