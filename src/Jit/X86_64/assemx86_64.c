/*
 * Intel x64 assembler
 * Intended to be called from C code
 */

#include <assert.h>
#include <utils.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <string.h>

#include "x86_64P.h"
#include "jitP.h"
#include "jit.h"

codeLblPo preamble(assemCtxPo ctx, int32 lclCount) {
  codeLblPo entry = defineLabel(ctx, "entry", ctx->pc);
  push(RG(RBP), ctx);
  mov(RG(RBP), RG(RSP), ctx);
  sub(RG(RSP), IM(lclCount), ctx);
  return entry;
}

retCode postamble(assemCtxPo ctx) {
  mov(RG(RSP), RG(RBP), ctx);
  pop(RG(RBP), ctx);
  rtn(ctx);
  return Ok;
}

void clearCodeCtxMaps(assemCtxPo ctx) {
//  ctx->usedRegs = 0;
}






