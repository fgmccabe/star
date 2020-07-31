//
// Created by Francis McCabe on 7/17/20.
//
#include "unitTests.h"
#include "x86_64P.h"

retCode checkResult(u8 *src, integer srcLen, x64CtxPo ctx);

static retCode test_adc() {
  x64CtxPo ctx = createCtx();

  adc(RG(R10), RG(RAX), ctx);
  adc(RG(RAX), RG(R10), ctx);
  adc(RG(R10), RG(R9), ctx);
  adc(RG(R9), RG(R10), ctx);

  adc(RG(R10), IM(0x11223344), ctx);
  adc(RG(RCX), IM(0x55667788), ctx);

  adc(RG(R10), BS(RAX, 0x11223344), ctx);
  adc(BS(RAX, 0x11223344), RG(R10), ctx);
  adc(BS(RCX, 0x11223344), IM(0x55667788), ctx);
  adc(BS(R10, 0x11223344), IM(0x55667788), ctx);
  adc(BS(R9, 0x11223344), IM(0x55667788), ctx);
  adc(BS(RAX, 0x11223344), IM(0x55667788), ctx);

  adc(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  adc(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
  adc(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  u8 tgt[] = {0x49, 0x11, 0xc2, // adc r10,rax
              0x4c, 0x11, 0xd0, // adc rax,r10
              0x4d, 0x11, 0xca, // adc r10,r9
              0x4d, 0x11, 0xd1, // adc r9,r10
              0x49, 0x81, 0xd2, 0x44, 0x33, 0x22, 0x11, //         	adcq	%r10,$287454020
              0x48, 0x81, 0xd1, 0x88, 0x77, 0x66, 0x55, //         	adcq	%rcx,$1432778632,
              0x4c, 0x13, 0x90, 0x44, 0x33, 0x22, 0x11, //         	adcq	%r10,287454020(%rax)
              0x4c, 0x11, 0x90, 0x44, 0x33, 0x22, 0x11, //        	adcq 287454020(%rax),%r10
              0x48, 0x81, 0x91, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // adcq	287454020(%rcx),$1432778632
              0x49, 0x81, 0x92, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // adcq	287454020(%r10),$1432778632
              0x49, 0x81, 0x91, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // adcq	287454020(%r9),$1432778632
              0x48, 0x81, 0x90, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // adcq	287454020(%rax),$1432778632
              0x4e, 0x13, 0x54, 0xd2, 0x55, //               	adcq	%rcx,85(%rdx,%r10,8)
              0x4e, 0x11, 0x54, 0xd2, 0x55,  //             	adcq	%r10, 85(%rdx,%r10,8)
              0x4a, 0x81, 0x54, 0xd2, 0x55, 0x44, 0x33, 0x22, 0x11, //  	adcq 85(%rdx,%r10,8),$287454020
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_add() {
  x64CtxPo ctx = createCtx();

  add(RG(R10), RG(RAX), ctx);
  add(RG(RAX), RG(R10), ctx);
  add(RG(R10), RG(R9), ctx);
  add(RG(R9), RG(R10), ctx);

  add(RG(R10), IM(0x11223344), ctx);
  add(RG(RCX), IM(0x55667788), ctx);

  add(RG(R10), BS(RAX, 0x11223344), ctx);
  add(BS(RAX, 0x11223344), RG(R10), ctx);
  add(BS(RCX, 0x11223344), IM(0x55667788), ctx);
  add(BS(R10, 0x11223344), IM(0x55667788), ctx);
  add(BS(R9, 0x11223344), IM(0x55667788), ctx);
  add(BS(RAX, 0x11223344), IM(0x55667788), ctx);

  add(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  add(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
  add(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  u8 tgt[] = {0x49, 0x01, 0xc2, // add r10,rax
              0x4c, 0x01, 0xd0, // add rax,r10
              0x4d, 0x01, 0xca, // add r10,r9
              0x4d, 0x01, 0xd1, // add r9,r10
              0x49, 0x81, 0xc2, 0x44, 0x33, 0x22, 0x11, //         	addq	%r10,$287454020
              0x48, 0x81, 0xc1, 0x88, 0x77, 0x66, 0x55, //         	addq	%rcx,$1432778632,
              0x4c, 0x03, 0x90, 0x44, 0x33, 0x22, 0x11, //         	addq	%r10,287454020(%rax)
              0x4c, 0x01, 0x90, 0x44, 0x33, 0x22, 0x11, //        	addq 287454020(%rax),%r10
              0x48, 0x81, 0x81, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // addq	287454020(%rcx),$1432778632
              0x49, 0x81, 0x82, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // addq	287454020(%r10),$1432778632
              0x49, 0x81, 0x81, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // addq	287454020(%r9),$1432778632
              0x48, 0x81, 0x80, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // addq	287454020(%rax),$1432778632
              0x4e, 0x03, 0x54, 0xd2, 0x55, //               	addq	%rcx,85(%rdx,%r10,8)
              0x4e, 0x01, 0x54, 0xd2, 0x55,  //             	addq	%r10, 85(%rdx,%r10,8)
              0x4a, 0x81, 0x44, 0xd2, 0x55, 0x44, 0x33, 0x22, 0x11, //  	addq 85(%rdx,%r10,8),$287454020
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_and() {
  x64CtxPo ctx = createCtx();

  and(RG(R10), RG(RAX), ctx);
  and(RG(RAX), RG(R10), ctx);
  and(RG(R10), RG(R9), ctx);
  and(RG(R9), RG(R10), ctx);

  and(RG(R10), IM(0x11223344), ctx);
  and(RG(RCX), IM(0x55667788), ctx);

  and(RG(R10), BS(RAX, 0x11223344), ctx);
  and(BS(RAX, 0x11223344), RG(R10), ctx);
  and(BS(RCX, 0x11223344), IM(0x55667788), ctx);
  and(BS(R10, 0x11223344), IM(0x55667788), ctx);
  and(BS(R9, 0x11223344), IM(0x55667788), ctx);
  and(BS(RAX, 0x11223344), IM(0x55667788), ctx);

  and(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  and(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
  and(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  u8 tgt[] = {0x49, 0x21, 0xc2, // and r10,rax
              0x4c, 0x21, 0xd0, // and rax,r10
              0x4d, 0x21, 0xca, // and r10,r9
              0x4d, 0x21, 0xd1, // and r9,r10
              0x49, 0x81, 0xe2, 0x44, 0x33, 0x22, 0x11, //         	andq	%r10,$287454020
              0x48, 0x81, 0xe1, 0x88, 0x77, 0x66, 0x55, //         	andq	%rcx,$1432778632,
              0x4c, 0x23, 0x90, 0x44, 0x33, 0x22, 0x11, //         	andq	%r10,287454020(%rax)
              0x4c, 0x21, 0x90, 0x44, 0x33, 0x22, 0x11, //        	andq 287454020(%rax),%r10
              0x48, 0x81, 0xa1, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // andq	287454020(%rcx),$1432778632
              0x49, 0x81, 0xa2, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // andq	287454020(%r10),$1432778632
              0x49, 0x81, 0xa1, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // andq	287454020(%r9),$1432778632
              0x48, 0x81, 0xa0, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // andq	287454020(%rax),$1432778632
              0x4e, 0x23, 0x54, 0xd2, 0x55, //               	andq	%rcx,85(%rdx,%r10,8)
              0x4e, 0x21, 0x54, 0xd2, 0x55,  //             	andq	%r10, 85(%rdx,%r10,8)
              0x4a, 0x81, 0x64, 0xd2, 0x55, 0x44, 0x33, 0x22, 0x11, //  	andq 85(%rdx,%r10,8),$287454020
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_jcc() {
  x64CtxPo ctx = createCtx();

  x64LblPo l0 = defineLabel(ctx, "l0", ctx->pc);
  x64LblPo l1 = defineLabel(ctx, "l1", -1);
  ja(l0, ctx);
  ja(l1, ctx);
  setLabel(ctx, l1);

  u8 tgt[] = {0x0f, 0x87, 0xfa, 0xff, 0xff, 0xff,
              0x0f, 0x87, 0x0, 0x0, 0x0, 0x0};

  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_jmp() {
  x64CtxPo ctx = createCtx();

  x64LblPo l0 = defineLabel(ctx, "l0", ctx->pc);
  x64LblPo l1 = defineLabel(ctx, "l1", -1);
  jmp(LB(l0), ctx);
  jmp(LB(l1), ctx);
  jmp(RG(R12), ctx);
  setLabel(ctx, l1);

  u8 tgt[] = {0xe9, 0xfb, 0xff, 0xff, 0xff,
              0xe9, 0x3, 0x0, 0x0, 0x0,
              0x41, 0xff, 0xe4};

  return checkResult(tgt, NumberOf(tgt), ctx);
}

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

static retCode test_setcc() {
  x64CtxPo ctx = createCtx();

  setne(RAX, ctx);
  setne(RCX, ctx);
  sets(RDX, ctx);

  u8 tgt[] = {0x0f, 0x95, 0xc0,
              0x0f, 0x95, 0xc1,
              0x0f, 0x98, 0xc2};

  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_testr() {
  x64CtxPo ctx = createCtx();

  test(RG(RAX), RG(RAX), ctx);
  test(RG(RCX), RG(RAX), ctx);
  test(RG(RDX), RG(RCX), ctx);
  test(RG(R10), RG(RAX), ctx);
  test(RG(RAX), RG(R10), ctx);
  test(RG(R12), RG(R10), ctx);

  u8 tgt[] = {0x48, 0x85, 0xc0,
              0x48, 0x85, 0xc1,
              0x48, 0x85, 0xca,
              0x49, 0x85, 0xc2,
              0x4c, 0x85, 0xd0,
              0x4d, 0x85, 0xd4
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_testrm() {
  x64CtxPo ctx = createCtx();

  test(RG(RAX), BS(RAX, 0x11223344), ctx);
  test(RG(RCX), BS(RAX, 0x11223344), ctx);
  test(RG(RDX), IX(RCX, RAX, 4, 0x55), ctx);
  test(RG(R10), IX(RCX, RAX, 4, 0x55), ctx);
  test(RG(RDX), IX(RCX, R10, 4, 0x55), ctx);

  test(RG(RAX), IM(0x11223344), ctx);
  test(RG(RDX), IM(0x55667788), ctx);
  test(RG(R10), IM(0x55667788), ctx);

  u8 tgt[] = {0x48, 0x85, 0x80, 0x44, 0x33, 0x22, 0x11,
              0x48, 0x85, 0x88, 0x44, 0x33, 0x22, 0x11,
              0x48, 0x85, 0x54, 0x81, 0x55,
              0x4c, 0x85, 0x54, 0x81, 0x55,
              0x4a, 0x85, 0x54, 0x91, 0x55,
              0x48, 0xa9, 0x44, 0x33, 0x22, 0x11,
              0x48, 0xf7, 0xc2, 0x88, 0x77, 0x66, 0x55,
              0x49, 0xf7, 0xc2, 0x88, 0x77, 0x66, 0x55
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_testmr() {
  x64CtxPo ctx = createCtx();

  test(BS(RAX, 0x11223344), RG(RAX), ctx);
  test(BS(RAX, 0x11223344), RG(RCX), ctx);
  test(IX(RCX, RAX, 4, 0x55), RG(RDX), ctx);
  test(IX(RCX, RAX, 4, 0x55), RG(R10), ctx);
  test(IX(RCX, R10, 4, 0x55), RG(RDX), ctx);

  test(IM(0x11223344), RG(RAX), ctx);
  test(IM(0x55667788), RG(R10), ctx);

  u8 tgt[] = {0x48, 0x85, 0x80, 0x44, 0x33, 0x22, 0x11, // testq	%rax, 287454020(%rax)
              0x48, 0x85, 0x88, 0x44, 0x33, 0x22, 0x11,
              0x48, 0x85, 0x54, 0x81, 0x55,
              0x4c, 0x85, 0x54, 0x81, 0x55,
              0x4a, 0x85, 0x54, 0x91, 0x55,
              0x48, 0xa9, 0x44, 0x33, 0x22, 0x11,
              0x49, 0xf7, 0xc2, 0x88, 0x77, 0x66, 0x55
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

static retCode test_xchg() {
  x64CtxPo ctx = createCtx();

  xchg(RG(R10), RG(RAX), ctx);
  xchg(RG(RAX), RG(R10), ctx);
  xchg(RG(R10), RG(R9), ctx);
  xchg(RG(R9), RG(R10), ctx);

  xchg(RG(R10), BS(RAX, 0x11223344), ctx);
  xchg(BS(RAX, 0x11223344), RG(R10), ctx);

  xchg(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  xchg(IX(RDX, R10, 8, 0x55), RG(R10), ctx);

  u8 tgt[] = {0x49, 0x92,
              0x49, 0x92,
              0x4d, 0x87, 0xca,
              0x4d, 0x87, 0xd1,
              0x4c, 0x87, 0x90, 0x44, 0x33, 0x22, 0x11,
              0x4c, 0x87, 0x90, 0x44, 0x33, 0x22, 0x11,
              0x4e, 0x87, 0x54, 0xd2, 0x55,
              0x4e, 0x87, 0x54, 0xd2, 0x55,
  };
  return checkResult(tgt, NumberOf(tgt), ctx);
}

retCode all_tests() {
  tests_run = 0;

  tryRet(run_test(test_adc));
  tryRet(run_test(test_add));
  tryRet(run_test(test_and));
  tryRet(run_test(test_jcc));
  tryRet(run_test(test_jmp));
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
  tryRet(run_test(test_setcc));
  tryRet(run_test(test_testr));
  tryRet(run_test(test_testrm));
  tryRet(run_test(test_testmr));
  tryRet(run_test(test_xchg));

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
