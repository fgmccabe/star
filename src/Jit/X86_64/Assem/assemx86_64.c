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






