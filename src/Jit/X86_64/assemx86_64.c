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

void initAssem() {
}

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
  ctx->usedRegs = 0;
  ctx->freeRegs = (1<<RAX)|(1<<RCX)|(1<RBX)|(1<<RSI)|(1<<RDI)|
    (1<<R8)|(1<<R9)|(1<<R10)|(1<<R11)|(1<<R12)|(1<<R13)|(1<<R14)|(1<<R15);
}






