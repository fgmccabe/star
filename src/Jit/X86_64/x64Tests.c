//
// Created by Francis McCabe on 7/17/20.
//
#include "unitTests.h"
#include "x86_64P.h"

static retCode test_lea() {
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

  lea(R12, BS(RAX, 0x11223344), ctx);       // lea %r12,0x123344(%rax)
  lea(R10, IX(RAX, R10, 8, 0x34), ctx);    // lea %r10,0x34(rax,r10*8)
  u8 tgt[] = {0x4c, 0x8d, 0xa0, 0x44, 0x33, 0x22, 0x11,
              0x4e, 0x8d, 0x54, 0xd0, 0x34};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movrr() {
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

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
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

  mov(RG(R12), IM(0x55), ctx);
  mov(RG(R12), IM(0x1122334455667788), ctx);
  u8 tgt[] = {0x49, 0xc7, 0xc4, 0x55, 0x00, 0x00, 0x00,
              0x49, 0xbc, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movrm() {
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

  mov(RG(RAX), BS(RAX, 0x55), ctx);
  mov(RG(RAX), BS(RSI, 0), ctx);
  u8 tgt[] = {0x48, 0x8b, 0x40, 0x55,
              0x48, 0x8b, 0x06};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movrx() {
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

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
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

  mov(BS(RDX, 0x0), RG(R12), ctx); // mov (rdx),r12
  mov(BS(RDX, 0x55), RG(R12), ctx); // mov 0x55(rdx),r12
  mov(BS(RDX, 0x11223344), RG(R12), ctx); // mov 0x11223344(rdx),r12
  u8 tgt[] = {0x4c, 0x89, 0x22,
              0x4c, 0x89, 0x62, 0x55,
              0x4c, 0x89, 0xa2, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movmi() {
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

  mov(BS(R12, 0x22), IM(0x11), ctx);
  mov(BS(RAX, 0x55), IM(0x11223344), ctx);
  u8 tgt[] = {0x49, 0xc7, 0x44, 0x24, 0x22, 0x11, 0x00, 0x00, 0x00,
              0x48, 0xc7, 0x40, 0x55, 0x44, 0x33, 0x22, 0x11};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_movxr() {
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

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
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

  mov(IX(R11, RDX, 8, 0x11223344), IM(0x55), ctx); // mov $0x55,0x11223344(r11,rdx*8)
  mov(IX(RAX, R11, 8, 0x11223344), IM(0x55), ctx); // mov $0x55,0x11223344(rax,rax*8)

  u8 tgt[] = {0x49, 0xc7, 0x84, 0xd3, 0x44, 0x33, 0x22, 0x11, 0x55, 0x00, 0x00, 0x00,
              0x4a, 0xc7, 0x84, 0xd8, 0x44, 0x33, 0x22, 0x11, 0x55, 0x00, 0x00, 0x00};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_popr() {
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

  pop(RG(RAX), ctx);
  pop(RG(RSI), ctx);
  pop(RG(R12), ctx);

  u8 tgt[] = {0x58,
              0x5e,
              0x41, 0x5c};
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_popb() {
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

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
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

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
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

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
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

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
  AssemCtxRecord Ctx;
  x64CtxPo ctx = setupCtx(&Ctx);

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
