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

void initAssem() {
}

void *createCode(assemCtxPo ctx) {
  cleanupLabels(ctx);
  void *code = mmap(Null, ctx->pc, PROT_EXEC | PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
  memcpy(code, ctx->bytes, ctx->pc);
  free(ctx->bytes);
  ctx->bytes = Null;
  discardCtx(ctx);
  return code;
}

static void encodeDPReg2Src(uint1 wide, uint1 S, uint8 op1, armReg R1, uint8 op2, armReg R2, armReg RD, assemCtxPo ctx)
){
uint32 ins = (wide << 31) | (S << 29) | (op1 << 21) | (R2 << 16) | (op2 << 10) | (R1 << 5) | RD;
emit32(ins, ctx
);
}

void clearCodeCtxMaps(assemCtxPo ctx) {
  ctx->usedRegs = 0;
  ctx->freeRegs = (1 << RAX) | (1 << RCX) | (1 < RBX) | (1 << RSI) | (1 << RDI) |
                  (1 << R8) | (1 << R9) | (1 << R10) | (1 << R11) | (1 << R12) | (1 << R13) | (1 << R14) | (1 << R15);
}

void adc_reg(uint1 wide, armReg RD, armReg S1, armReg S2, assemCtxPo ctx) {
  encodeDPReg2Src(wide, 0, 0xd0, S1, 0x0, S2, RD, ctx);
}

void adcs_reg(uint1 wide, armReg RD, armReg S1, armReg S2, assemCtxPo ctx) {
  encodeDPReg2Src(wide, 1, 0xd0, S1, 0x0, S2, RD, ctx);
}

typedef enum{
  ET_XTB = 0,
  ET_XTH = 1,
  ET_XTW = 2,
  ET_XTX = 3
} ExtendType;

void adx_reg(uint1 wide, armReg RD, uint1 sign,ExtendType et, uint8 shift, armReg S1, armReg S2, assemCtxPo ctx) {
  encodeDPReg2Src(wide, 0, 0x59, S1, ((usign<<5)|((et & 0x3) << 3) | ((shift & 0x7))), S2, RD, ctx);
}



