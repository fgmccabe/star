//
// Created by Francis McCabe on 7/17/20.
//
#include "unitTests.h"
#include "x86_64P.h"
#include <stdlib.h>

retCode checkResult(u8 *src, integer srcLen, x64CtxPo ctx);

static retCode test_lea() {
  x64CtxPo ctx = createCtx();

  lea(R12, BS(RAX, 0x11223344), ctx);       // lea %r12,0x123344(%rax)
  lea(R10, IX(RAX, R10, 8, 0x34), ctx);    // lea %r10,0x34(rax,r10*8)
  u8 tgt[] = {0x4c, 0x8d, 0xa0, 0x44, 0x33, 0x22, 0x11,
              0x4e, 0x8d, 0x54, 0xd0, 0x34};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_lbl_lea() {
  x64CtxPo ctx = createCtx();

  x64LblPo l0 = defineLabel(ctx, "l0", 0);
  x64LblPo l1 = defineLabel(ctx, "l1", -1);
  lea(R12, LB(l1), ctx); // lea %r12,l1(%rip)
  lea(R10, LB(l0), ctx); // lea %r10,l0(%rip)
  setLabel(ctx, l1);
  u8 tgt[] = {0x4c, 0x8d, 0x25, 0x07, 0x0, 0x0, 0x0,
              0x4c, 0x8d, 0x15, 0xf2, 0xff, 0xff, 0xff};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movrr() {
  x64CtxPo ctx = createCtx();

  mov(RG(RSI), RG(RAX), ctx);
  mov(RG(RAX), RG(RSI), ctx);

  mov(RG(R12), RG(RSI), ctx);
  mov(RG(RSI), RG(R12), ctx);

  mov(RG(R12), RG(R10), ctx);

  u8 tgt[] = {0x48, 0x89, 0xc6,
              0x48, 0x89, 0xf0,
              0x49, 0x89, 0xf4,
              0x4c, 0x89, 0xe6,
              0x4d, 0x89, 0xd4};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movri() {
  x64CtxPo ctx = createCtx();

  mov(RG(R12), IM(0x55), ctx);
  mov(RG(R12), IM(0x1122334455667788), ctx);
  u8 tgt[] = {0x49, 0xc7, 0xc4, 0x55, 0x00, 0x00, 0x00,
              0x49, 0xbc, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movrm() {
  x64CtxPo ctx = createCtx();

  mov(RG(RAX), BS(RAX, 0x55), ctx);
  mov(RG(RAX), BS(RSI, 0), ctx);
  u8 tgt[] = {0x48, 0x8b, 0x40, 0x55,
              0x48, 0x8b, 0x06};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movrx() {
  x64CtxPo ctx = createCtx();

  mov(RG(RAX), IX(RAX, R12, 4, 0x55), ctx);
  mov(RG(RAX), IX(RAX, RDX, 8, 0x55), ctx);
  mov(RG(RAX), IX(RBX, RDX, 8, 0x55), ctx);
  mov(RG(RAX), IX(RBX, R12, 4, 0x55), ctx);

  u8 tgt[] = {0x4a, 0x8b, 0x44, 0xa0, 0x55,
              0x48, 0x8b, 0x44, 0xd0, 0x55,
              0x48, 0x8b, 0x44, 0xd3, 0x55,
              0x4a, 0x8b, 0x44, 0xa3, 0x55};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movmr() {
  x64CtxPo ctx = createCtx();

  mov(BS(RDX, 0x0), RG(R12), ctx); // mov (rdx),r12
  mov(BS(RDX, 0x55), RG(R12), ctx); // mov 0x55(rdx),r12
  mov(BS(RDX, 0x11223344), RG(R12), ctx); // mov 0x11223344(rdx),r12
  u8 tgt[] = {0x4c, 0x89, 0x22,
              0x4c, 0x89, 0x62, 0x55,
              0x4c, 0x89, 0xa2, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movmi() {
  x64CtxPo ctx = createCtx();

  mov(BS(R12, 0x22), IM(0x11), ctx);
  mov(BS(RAX, 0x55), IM(0x11223344), ctx);
  u8 tgt[] = {0x49, 0xc7, 0x44, 0x24, 0x22, 0x11, 0x00, 0x00, 0x00,
              0x48, 0xc7, 0x40, 0x55, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movxr() {
  x64CtxPo ctx = createCtx();

  mov(IX(RAX, RDX, 8, 0x11223344), RG(RAX), ctx);
  mov(IX(R11, RDX, 8, 0x11223344), RG(RAX), ctx);
  mov(IX(R11, RDX, 4, 0x11223344), RG(RAX), ctx);
  mov(IX(RAX, R12, 4, 0x55), RG(RAX), ctx); // mov 0x55(rax,r12*4),rax
  mov(IX(RBX, RDX, 8, 0x55), RG(RAX), ctx); // mov 0x55(rbx, rdx*8),rax
  mov(IX(RBX, R12, 4, 0x55), RG(RAX), ctx); // mov 0x55(rbx, r12*4), rax

  u8 tgt[] = {0x48, 0x89, 0x84, 0xd0, 0x44, 0x33, 0x22, 0x11,
              0x49, 0x89, 0x84, 0xd3, 0x44, 0x33, 0x22, 0x11,
              0x49, 0x89, 0x84, 0x93, 0x44, 0x33, 0x22, 0x11,
              0x4a, 0x89, 0x44, 0xa0, 0x55,
              0x48, 0x89, 0x44, 0xd3, 0x55,
              0x4a, 0x89, 0x44, 0xa3, 0x55};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movxi() {
  x64CtxPo ctx = createCtx();

  mov(IX(R11, RDX, 8, 0x11223344), IM(0x55), ctx); // mov $0x55,0x11223344(r11,rdx*8)
  mov(IX(RAX, R11, 8, 0x11223344), IM(0x55), ctx); // mov $0x55,0x11223344(rax,rax*8)

  u8 tgt[] = {0x49, 0xc7, 0x84, 0xd3, 0x44, 0x33, 0x22, 0x11, 0x55, 0x00, 0x00, 0x00,
              0x4a, 0xc7, 0x84, 0xd8, 0x44, 0x33, 0x22, 0x11, 0x55, 0x00, 0x00, 0x00};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_popr() {
  x64CtxPo ctx = createCtx();

  pop(RG(RAX), ctx);
  pop(RG(RSI), ctx);
  pop(RG(R12), ctx);

  u8 tgt[] = {0x58,
              0x5e,
              0x41, 0x5c};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_popb() {
  x64CtxPo ctx = createCtx();

  pop(BS(RAX, 0x55), ctx);
  pop(BS(RAX, 0x11223344), ctx);

  pop(BS(R10, 0x55), ctx);
  pop(BS(R10, 0x11223344), ctx);

  u8 tgt[] = {0x8f, 0x40, 0x55,
              0x8f, 0x80, 0x44, 0x33, 0x22, 0x11,
              0x41, 0x8f, 0x42, 0x55,
              0x41, 0x8f, 0x82, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_popx() {
  x64CtxPo ctx = createCtx();

  pop(IX(RAX, RDX, 4, 0x55), ctx); // pop 0x55(rax,r10*4)
  pop(IX(RDX, RAX, 4, 0x11223344), ctx); // pop 0x11223344(rdx,rax*4)
  pop(IX(RAX, R10, 4, 0x55), ctx); // pop 0x55(rax,r10*4)
  pop(IX(R12, R10, 4, 0x55), ctx); // pop 055(r12,r10*4)
  pop(IX(R12, RAX, 4, 0x55), ctx); // pop 0x55(r12,rax*4)

  u8 tgt[] = {0x8f, 0x44, 0x90, 0x55,
              0x8f, 0x84, 0x82, 0x44, 0x33, 0x22, 0x11,
              0x42, 0x8f, 0x44, 0x90, 0x55,
              0x43, 0x8f, 0x44, 0x94, 0x55,
              0x41, 0x8f, 0x44, 0x84, 0x55,
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_pushri() {
  x64CtxPo ctx = createCtx();

  push(RG(RAX), ctx);
  push(RG(RSI), ctx);
  push(RG(R12), ctx);

  push(IM(0x55), ctx);
  push(IM(0x11223344), ctx);

  u8 tgt[] = {0x50,
              0x56,
              0x41, 0x54,
              0x6a, 0x55,
              0x68, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_pushb() {
  x64CtxPo ctx = createCtx();

  push(BS(RAX, 0x55), ctx);
  push(BS(RAX, 0x11223344), ctx);

  push(BS(R10, 0x55), ctx);
  push(BS(R10, 0x11223344), ctx);

  u8 tgt[] = {0xff, 0x70, 0x55,
              0xff, 0xb0, 0x44, 0x33, 0x22, 0x11,
              0x41, 0xff, 0x72, 0x55,
              0x41, 0xff, 0xb2, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_pushx() {
  x64CtxPo ctx = createCtx();

  push(IX(RAX, RDX, 4, 0x55), ctx);
  push(IX(RAX, RDX, 4, 0x11223344), ctx);
  push(IX(RAX, R10, 4, 0x55), ctx);
  push(IX(R12, R10, 4, 0x55), ctx);
  push(IX(R12, RAX, 4, 0x55), ctx);

  u8 tgt[] = {0xff, 0x74, 0x90, 0x55,
              0xff, 0xb4, 0x90, 0x44, 0x33, 0x22, 0x11,
              0x42, 0xff, 0x74, 0x90, 0x55,
              0x43, 0xff, 0x74, 0x94, 0x55,
              0x41, 0xff, 0x74, 0x84, 0x55,
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

retCode all_tests() {
  tests_run = 0;

  tryRet(run_test(test_lea));
  tryRet(run_test(test_lbl_lea));
  tryRet(run_test(test_movrr));
  tryRet(run_test(test_movri));
  tryRet(run_test(test_movrm));
  tryRet(run_test(test_movrx));
  tryRet(run_test(test_movmr));
  tryRet(run_test(test_movmi));
  tryRet(run_test(test_movxr));
  tryRet(run_test(test_movxi));
  tryRet(run_test(test_popr));
  tryRet(run_test(test_popb));
  tryRet(run_test(test_popx));
  tryRet(run_test(test_pushri));
  tryRet(run_test(test_pushb));
  tryRet(run_test(test_pushx));

  return Ok;
}

retCode checkResult(u8 *src, integer srcLen, x64CtxPo ctx) {
  retCode ret;
  if (ctx->pc != srcLen) {
    logMsg(logFile, "%d bytes expected, %d bytes generated", srcLen, ctx->pc);
    logMsg(logFile, "actual bytes: %.*X", ctx->pc, ctx->bytes);
    ret = Error;
  } else
    ret = cmpBytes(src, ctx->bytes, srcLen);
  cleanupCtx(ctx);
  return ret;
}
