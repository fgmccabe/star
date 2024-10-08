//
// Created by Francis McCabe on 7/17/20.
//
#include "unitTests.h"
#include "x86_64P.h"
#include "jitP.h"

static retCode checkCode(uint8 *src, integer srcLen, assemCtxPo ctx);

static retCode test_adc() {
  assemCtxPo ctx = createCtx();

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

  uint8 tgt[] = {0x49, 0x11, 0xc2, // adc r10,rax
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
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_add() {
  assemCtxPo ctx = createCtx();

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

  uint8 tgt[] = {0x49, 0x01, 0xc2, // add r10,rax
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
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_and() {
  assemCtxPo ctx = createCtx();

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

  uint8 tgt[] = {0x49, 0x21, 0xc2, // and r10,rax
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
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_call() {
  assemCtxPo ctx = createCtx();

  codeLblPo l0 = defineLabel(ctx, "l0", ctx->pc);
  codeLblPo l1 = defineLabel(ctx, "l1", undefinedPc);
  call(LB(l0), ctx);
  call(LB(l1), ctx);
  call(RG(R12), ctx);
  call(RG(RAX), ctx);
  setLabel(ctx, l1);

  uint8 tgt[] = {0xe8, 0xfb, 0xff, 0xff, 0xff,
                 0xe8, 0x5, 0x0, 0x0, 0x0,
                 0x41, 0xff, 0xd4,
                 0xff, 0xd0};

  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_ret() {
  assemCtxPo ctx = createCtx();

  ret(0, ctx);
  ret(0x3344, ctx);

  uint8 tgt[] = {0xc3,
                 0xc2, 0x44, 0x33
  };

  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_cmp() {
  assemCtxPo ctx = createCtx();

  cmp(RG(R10), RG(RAX), ctx);
  cmp(RG(RAX), RG(R10), ctx);
  cmp(RG(R10), RG(R9), ctx);
  cmp(RG(R9), RG(R10), ctx);

  cmp(RG(R10), IM(0x11223344), ctx);
  cmp(RG(RCX), IM(0x55667788), ctx);

  cmp(RG(R10), BS(RAX, 0x11223344), ctx);
  cmp(BS(RAX, 0x11223344), RG(R10), ctx);
  cmp(BS(RCX, 0x11223344), IM(0x55667788), ctx);
  cmp(BS(R10, 0x11223344), IM(0x55667788), ctx);
  cmp(BS(R9, 0x11223344), IM(0x55667788), ctx);
  cmp(BS(RAX, 0x11223344), IM(0x55667788), ctx);

  cmp(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  cmp(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
  cmp(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  uint8 tgt[] = {0x49, 0x39, 0xc2, // cmp r10,rax
                 0x4c, 0x39, 0xd0, // cmp rax,r10
                 0x4d, 0x39, 0xca, // cmp r10,r9
                 0x4d, 0x39, 0xd1, // cmp r9,r10
                 0x49, 0x81, 0xfa, 0x44, 0x33, 0x22, 0x11, //         	cmpq	%r10,$287454020
                 0x48, 0x81, 0xf9, 0x88, 0x77, 0x66, 0x55, //         	cmpq	%rcx,$1432778632,
                 0x4c, 0x3b, 0x90, 0x44, 0x33, 0x22, 0x11, //         	cmpq	%r10,287454020(%rax)
                 0x4c, 0x39, 0x90, 0x44, 0x33, 0x22, 0x11, //        	cmpq 287454020(%rax),%r10
                 0x48, 0x81, 0xb9, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // cmpq	287454020(%rcx),$1432778632
                 0x49, 0x81, 0xba, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // cmpq	287454020(%r10),$1432778632
                 0x49, 0x81, 0xb9, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // cmpq	287454020(%r9),$1432778632
                 0x48, 0x81, 0xb8, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // cmpq	287454020(%rax),$1432778632
                 0x4e, 0x3b, 0x54, 0xd2, 0x55, //               	cmpq	%rcx,85(%rdx,%r10,8)
                 0x4e, 0x39, 0x54, 0xd2, 0x55,  //             	cmpq	%r10, 85(%rdx,%r10,8)
                 0x4a, 0x81, 0x7c, 0xd2, 0x55, 0x44, 0x33, 0x22, 0x11, //  	cmpq 85(%rdx,%r10,8),$287454020
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_dec() {
  assemCtxPo ctx = createCtx();

  dec(RG(RAX), ctx);
  dec(RG(R10), ctx);
  dec(RG(RCX), ctx);

  dec(BS(RAX, 0x11223344), ctx);
  dec(BS(R10, 0x11223344), ctx);
  dec(BS(RCX, 0x11223344), ctx);

  dec(IX(RAX, R10, 8, 0x55), ctx);
  dec(IX(R10, R10, 8, 0x55), ctx);
  dec(IX(RCX, R9, 8, 0x55), ctx);

  uint8 tgt[] = {0x48, 0xff, 0xc8,  // dec	%rax
                 0x49, 0xff, 0xca,  // dec	%r10
                 0x48, 0xff, 0xc9,  // dec %rcx
                 0x48, 0xff, 0x88, 0x44, 0x33, 0x22, 0x11, // dec 287454020(rax)
                 0x49, 0xff, 0x8a, 0x44, 0x33, 0x22, 0x11, // dec 287454020(r10)
                 0x48, 0xff, 0x89, 0x44, 0x33, 0x22, 0x11, // dec 287454020(rcx)
                 0x4a, 0xff, 0x4c, 0xd0, 0x55, // decq	85(%rax,%r10,8)
                 0x4b, 0xff, 0x4c, 0xd2, 0x55, //  decq	85(%r10,%r10,8)
                 0x4a, 0xff, 0x4c, 0xc9, 0x55  //  decq	85(%rcx,%r9,8)
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_idiv() {
  assemCtxPo ctx = createCtx();

  codeLblPo l0 = defineLabel(ctx, "l0", ctx->pc);

  idiv(RG(RAX), ctx);
  idiv(RG(R10), ctx);

  idiv(BS(R9, 0x11223344), ctx);

  idiv(IX(RDX, R10, 8, 0x55), ctx);

  idiv(LB(l0), ctx);

  uint8 tgt[] = {0x48, 0xf7, 0xf8,  // idivq	%rax
                 0x49, 0xf7, 0xfa,  // idivq	%r10
                 0x49, 0xf7, 0xb9, 0x44, 0x33, 0x22, 0x11, //      	idivq	287454020(%rax)
                 0x4a, 0xf7, 0x7c, 0xd2, 0x55, //            	idivq	85(%rdx,%r10,8)
                 0x48, 0xf7, 0x3d, 0xe7, 0xff, 0xff, 0xff, //      	idivq	l0(%rip)
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_imul() {
  assemCtxPo ctx = createCtx();
  codeLblPo l0 = defineLabel(ctx, "l0", ctx->pc);

  imul(R10, RG(RAX), ctx);
  imul(RAX, RG(R10), ctx);
  imul(R10, RG(R9), ctx);
  imul(R9, RG(R10), ctx);

  imul(R10, IM(0x55667788), ctx);
  imul(RCX, IM(0x55667788), ctx);

  imul(R10, BS(RAX, 0x11223344), ctx);

  imul(R10, IX(RDX, R10, 8, 0x55), ctx);

  imul(R10, LB(l0), ctx);

  uint8 tgt[] = {0x4c, 0x0f, 0xaf, 0xd0, //                  	imulq	%rax, %r10
                 0x49, 0x0f, 0xaf, 0xc2, //                	imulq	%r10, %rax
                 0x4d, 0x0f, 0xaf, 0xd1, //                  	imulq	%r9, %r10
                 0x4d, 0x0f, 0xaf, 0xca, //                  	imulq	%r10, %r9
                 0x4d, 0x69, 0xd2, 0x88, 0x77, 0x66, 0x55, //         	imulq	$1432778632, %r10, %r10
                 0x48, 0x69, 0xc9, 0x88, 0x77, 0x66, 0x55, //         	imulq	$1432778632, %rcx, %rcx
                 0x4c, 0x0f, 0xaf, 0x90, 0x44, 0x33, 0x22, 0x11, //      	imulq	287454020(%rax), %r10
                 0x4e, 0x0f, 0xaf, 0x54, 0xd2, 0x55, //            	imulq	85(%rdx,%r10,8), %r10
                 0x4c, 0x0f, 0xaf, 0x15, 0xcc, 0xff, 0xff, 0xff, //      	imulq	l0(%rip), %r10
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_inc() {
  assemCtxPo ctx = createCtx();

  inc(RG(RAX), ctx);
  inc(RG(R10), ctx);
  inc(RG(RCX), ctx);

  inc(BS(RAX, 0x11223344), ctx);
  inc(BS(R10, 0x11223344), ctx);
  inc(BS(RCX, 0x11223344), ctx);

  inc(IX(RAX, R10, 8, 0x55), ctx);
  inc(IX(R10, R10, 8, 0x55), ctx);
  inc(IX(RCX, R9, 8, 0x55), ctx);

  uint8 tgt[] = {0x48, 0xff, 0xc0,  // inc	%rax
                 0x49, 0xff, 0xc2,  // inc	%r10
                 0x48, 0xff, 0xc1,  // inc %rcx
                 0x48, 0xff, 0x80, 0x44, 0x33, 0x22, 0x11, // inc 287454020(rax)
                 0x49, 0xff, 0x82, 0x44, 0x33, 0x22, 0x11, // inc 287454020(r10)
                 0x48, 0xff, 0x81, 0x44, 0x33, 0x22, 0x11, // inc 287454020(rcx)
                 0x4a, 0xff, 0x44, 0xd0, 0x55, // incq	85(%rax,%r10,8)
                 0x4b, 0xff, 0x44, 0xd2, 0x55, //  incq	85(%r10,%r10,8)
                 0x4a, 0xff, 0x44, 0xc9, 0x55  //  incq	85(%rcx,%r9,8)
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_jcc() {
  assemCtxPo ctx = createCtx();

  codeLblPo l0 = defineLabel(ctx, "l0", ctx->pc);
  codeLblPo l1 = defineLabel(ctx, "l1", undefinedPc);
  ja(l0, ctx);
  ja(l1, ctx);
  setLabel(ctx, l1);

  uint8 tgt[] = {0x0f, 0x87, 0xfa, 0xff, 0xff, 0xff,
                 0x0f, 0x87, 0x0, 0x0, 0x0, 0x0};

  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_jmp() {
  assemCtxPo ctx = createCtx();

  codeLblPo l0 = defineLabel(ctx, "l0", ctx->pc);
  codeLblPo l1 = defineLabel(ctx, "l1", undefinedPc);
  jmp(LB(l0), ctx);
  jmp(LB(l1), ctx);
  jmp(RG(R12), ctx);
  setLabel(ctx, l1);

  uint8 tgt[] = {0xe9, 0xfb, 0xff, 0xff, 0xff,
                 0xe9, 0x3, 0x0, 0x0, 0x0,
                 0x41, 0xff, 0xe4};

  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_lea() {
  assemCtxPo ctx = createCtx();

  lea(R12, BS(RAX, 0x11223344), ctx);       // lea %r12,0x123344(%rax)
  lea(R10, IX(RAX, R10, 8, 0x34), ctx);    // lea %r10,0x34(rax,r10*8)
  uint8 tgt[] = {0x4c, 0x8d, 0xa0, 0x44, 0x33, 0x22, 0x11,
                 0x4e, 0x8d, 0x54, 0xd0, 0x34};
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_lbl_lea() {
  assemCtxPo ctx = createCtx();

  codeLblPo l0 = defineLabel(ctx, "l0", 0);
  codeLblPo l1 = defineLabel(ctx, "l1", undefinedPc);
  lea(R12, LB(l1), ctx); // lea %r12,l1(%rip)
  lea(R10, LB(l0), ctx); // lea %r10,l0(%rip)
  setLabel(ctx, l1);
  uint8 tgt[] = {0x4c, 0x8d, 0x25, 0x07, 0x0, 0x0, 0x0,
                 0x4c, 0x8d, 0x15, 0xf2, 0xff, 0xff, 0xff};
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_mov() {
  assemCtxPo ctx = createCtx();

  mov(RG(RSI), RG(RAX), ctx);
  mov(RG(RAX), RG(RSI), ctx);
  mov(RG(R12), RG(RSI), ctx);
  mov(RG(RSI), RG(R12), ctx);
  mov(RG(R12), RG(R10), ctx);

  mov(RG(R12), IM(0x55), ctx);
  mov(RG(R12), IM(0x1122334455667788), ctx);

  mov(RG(RAX), BS(RAX, 0x55), ctx);
  mov(RG(RAX), BS(RSI, 0), ctx);

  mov(RG(RAX), IX(RAX, R12, 4, 0x55), ctx);
  mov(RG(RAX), IX(RAX, RDX, 8, 0x55), ctx);
  mov(RG(RAX), IX(RBX, RDX, 8, 0x55), ctx);
  mov(RG(RAX), IX(RBX, R12, 4, 0x55), ctx);

  mov(BS(RDX, 0x0), RG(R12), ctx); // mov (rdx),r12
  mov(BS(RDX, 0x55), RG(R12), ctx); // mov 0x55(rdx),r12
  mov(BS(RDX, 0x11223344), RG(R12), ctx); // mov 0x11223344(rdx),r12

  mov(BS(R12, 0x22), IM(0x11), ctx);
  mov(BS(RAX, 0x55), IM(0x11223344), ctx);

  mov(IX(RAX, RDX, 8, 0x11223344), RG(RAX), ctx);
  mov(IX(R11, RDX, 8, 0x11223344), RG(RAX), ctx);
  mov(IX(R11, RDX, 4, 0x11223344), RG(RAX), ctx);
  mov(IX(RAX, R12, 4, 0x55), RG(RAX), ctx); // mov 0x55(rax,r12*4),rax
  mov(IX(RBX, RDX, 8, 0x55), RG(RAX), ctx); // mov 0x55(rbx, rdx*8),rax
  mov(IX(RBX, R12, 4, 0x55), RG(RAX), ctx); // mov 0x55(rbx, r12*4), rax

  mov(IX(R11, RDX, 8, 0x11223344), IM(0x55), ctx); // mov $0x55,0x11223344(r11,rdx*8)
  mov(IX(RAX, R11, 8, 0x11223344), IM(0x55), ctx); // mov $0x55,0x11223344(rax,rax*8)


  uint8 tgt[] = {0x48, 0x89, 0xc6,
                 0x48, 0x89, 0xf0,
                 0x49, 0x89, 0xf4,
                 0x4c, 0x89, 0xe6,
                 0x4d, 0x89, 0xd4,
                 0x49, 0xc7, 0xc4, 0x55, 0x00, 0x00, 0x00,
                 0x49, 0xbc, 0x88, 0x77, 0x66, 0x55, 0x44, 0x33, 0x22, 0x11,
                 0x48, 0x8b, 0x40, 0x55,
                 0x48, 0x8b, 0x06,
                 0x4a, 0x8b, 0x44, 0xa0, 0x55,
                 0x48, 0x8b, 0x44, 0xd0, 0x55,
                 0x48, 0x8b, 0x44, 0xd3, 0x55,
                 0x4a, 0x8b, 0x44, 0xa3, 0x55,
                 0x4c, 0x89, 0x22,
                 0x4c, 0x89, 0x62, 0x55,
                 0x4c, 0x89, 0xa2, 0x44, 0x33, 0x22, 0x11,
                 0x49, 0xc7, 0x44, 0x24, 0x22, 0x11, 0x00, 0x00, 0x00,
                 0x48, 0xc7, 0x40, 0x55, 0x44, 0x33, 0x22, 0x11,
                 0x48, 0x89, 0x84, 0xd0, 0x44, 0x33, 0x22, 0x11,
                 0x49, 0x89, 0x84, 0xd3, 0x44, 0x33, 0x22, 0x11,
                 0x49, 0x89, 0x84, 0x93, 0x44, 0x33, 0x22, 0x11,
                 0x4a, 0x89, 0x44, 0xa0, 0x55,
                 0x48, 0x89, 0x44, 0xd3, 0x55,
                 0x4a, 0x89, 0x44, 0xa3, 0x55,
                 0x49, 0xc7, 0x84, 0xd3, 0x44, 0x33, 0x22, 0x11, 0x55, 0x00, 0x00, 0x00,
                 0x4a, 0xc7, 0x84, 0xd8, 0x44, 0x33, 0x22, 0x11, 0x55, 0x00, 0x00, 0x00
  };

  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_movsx() {
  assemCtxPo ctx = createCtx();

  codeLblPo l0 = defineLabel(ctx, "", ctx->pc);
  movsx(RSI, RG(RAX), 1, ctx);
  movsx(RAX, RG(RSI), 2, ctx);
  movsx(R12, RG(RSI), 4, ctx);

  movsx(R12, BS(RAX, 0x55), 2, ctx);
  movsx(RAX, BS(RSI, 0), 4, ctx);

  movsx(RAX, IX(RAX, R12, 4, 0x55), 1, ctx);
  movsx(R10, IX(RAX, RDX, 8, 0x55), 4, ctx);

  movsx(RAX, LB(l0), 4, ctx);
  movsx(R9, LB(l0), 2, ctx);

  uint8 tgt[] = {0x48, 0x0f, 0xbe, 0xf0,
                 0x48, 0x0f, 0xbf, 0xc6,
                 0x4c, 0x63, 0xe6,
                 0x4c, 0x0f, 0xbf, 0x60, 0x55,
                 0x48, 0x63, 0x06,
                 0x4a, 0x0f, 0xbe, 0x44, 0xa0, 0x55,
                 0x4c, 0x63, 0x54, 0xd0, 0x55,
                 0x48, 0x63, 0x05, 0xdb, 0xff, 0xff, 0xff,
                 0x4c, 0x0f, 0xbf, 0x0d, 0xd3, 0xff, 0xff, 0xff
  };

  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_neg() {
  assemCtxPo ctx = createCtx();

  neg(RG(RAX), ctx);
  neg(RG(R10), ctx);

  neg(BS(RAX, 0x11223344), ctx);
  neg(BS(R10, 0x11223344), ctx);

  neg(IX(RDX, R10, 8, 0x55), ctx);
  neg(IX(RAX, R10, 8, 0x55), ctx);
  neg(IX(RDX, RAX, 8, 0x55), ctx);
  neg(IX(RDX, RCX, 8, 0x55), ctx);

  uint8 tgt[] = {0x48, 0xf7, 0xd8, // neg rax
                 0x49, 0xf7, 0xda, // neg r10
                 0x48, 0xf7, 0x98, 0x44, 0x33, 0x22, 0x11, //         	negq	287454020(%rax)
                 0x49, 0xf7, 0x9a, 0x44, 0x33, 0x22, 0x11, //        	negq 287454020(%r10)
                 0x4a, 0xf7, 0x5c, 0xd2, 0x55, //  	negq 85(%rdx,%r10,8)
                 0x4a, 0xf7, 0x5c, 0xd0, 0x55, //    negq	85(%rax,%r10,8)
                 0x48, 0xf7, 0x5c, 0xc2, 0x55, //    negq	85(%rdx,%rax,8)
                 0x48, 0xf7, 0x5c, 0xca, 0x55, //    negq	85(%rdx,%rcx,8)
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_not() {
  assemCtxPo ctx = createCtx();

  not(RG(RAX), ctx);
  not(RG(R10), ctx);

  not(BS(RAX, 0x11223344), ctx);
  not(BS(R10, 0x11223344), ctx);

  not(IX(RDX, R10, 8, 0x55), ctx);
  not(IX(RAX, R10, 8, 0x55), ctx);
  not(IX(RDX, RAX, 8, 0x55), ctx);
  not(IX(RDX, RCX, 8, 0x55), ctx);

  uint8 tgt[] = {0x48, 0xf7, 0xd0, // not rax
                 0x49, 0xf7, 0xd2, // not r10
                 0x48, 0xf7, 0x90, 0x44, 0x33, 0x22, 0x11, //         	notq	287454020(%rax)
                 0x49, 0xf7, 0x92, 0x44, 0x33, 0x22, 0x11, //        	notq 287454020(%r10)
                 0x4a, 0xf7, 0x54, 0xd2, 0x55, //  	notq 85(%rdx,%r10,8)
                 0x4a, 0xf7, 0x54, 0xd0, 0x55, //    notq	85(%rax,%r10,8)
                 0x48, 0xf7, 0x54, 0xc2, 0x55, //    notq	85(%rdx,%rax,8)
                 0x48, 0xf7, 0x54, 0xca, 0x55, //    otq	85(%rdx,%rcx,8)
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_or() {
  assemCtxPo ctx = createCtx();

  or(RG(R10), RG(RAX), ctx);
  or(RG(RAX), RG(R10), ctx);
  or(RG(R10), RG(R9), ctx);
  or(RG(R9), RG(R10), ctx);

  or(RG(R10), IM(0x11223344), ctx);
  or(RG(RCX), IM(0x55667788), ctx);

  or(RG(R10), BS(RAX, 0x11223344), ctx);
  or(BS(RAX, 0x11223344), RG(R10), ctx);
  or(BS(RCX, 0x11223344), IM(0x55667788), ctx);
  or(BS(R10, 0x11223344), IM(0x55667788), ctx);
  or(BS(R9, 0x11223344), IM(0x55667788), ctx);
  or(BS(RAX, 0x11223344), IM(0x55667788), ctx);

  or(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  or(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
  or(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  uint8 tgt[] = {0x49, 0x09, 0xc2, // or r10,rax
                 0x4c, 0x09, 0xd0, // or rax,r10
                 0x4d, 0x09, 0xca, // or r10,r9
                 0x4d, 0x09, 0xd1, // or r9,r10
                 0x49, 0x81, 0xca, 0x44, 0x33, 0x22, 0x11, //         	orq	%r10,$287454020
                 0x48, 0x81, 0xc9, 0x88, 0x77, 0x66, 0x55, //         	orq	%rcx,$1432778632,
                 0x4c, 0x0b, 0x90, 0x44, 0x33, 0x22, 0x11, //         	orq	%r10,287454020(%rax)
                 0x4c, 0x09, 0x90, 0x44, 0x33, 0x22, 0x11, //        	orq 287454020(%rax),%r10
                 0x48, 0x81, 0x89, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // orq	287454020(%rcx),$1432778632
                 0x49, 0x81, 0x8a, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // orq	287454020(%r10),$1432778632
                 0x49, 0x81, 0x89, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // orq	287454020(%r9),$1432778632
                 0x48, 0x81, 0x88, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // orq	287454020(%rax),$1432778632
                 0x4e, 0x0b, 0x54, 0xd2, 0x55, //               	orq	%rcx,85(%rdx,%r10,8)
                 0x4e, 0x09, 0x54, 0xd2, 0x55,  //             	orq	%r10, 85(%rdx,%r10,8)
                 0x4a, 0x81, 0x4c, 0xd2, 0x55, 0x44, 0x33, 0x22, 0x11, //  	orq 85(%rdx,%r10,8),$287454020
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_popr() {
  assemCtxPo ctx = createCtx();

  pop(RG(RAX), ctx);
  pop(RG(RSI), ctx);
  pop(RG(R12), ctx);

  uint8 tgt[] = {0x58,
                 0x5e,
                 0x41, 0x5c};
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_popb() {
  assemCtxPo ctx = createCtx();

  pop(BS(RAX, 0x55), ctx);
  pop(BS(RAX, 0x11223344), ctx);

  pop(BS(R10, 0x55), ctx);
  pop(BS(R10, 0x11223344), ctx);

  uint8 tgt[] = {0x8f, 0x40, 0x55,
                 0x8f, 0x80, 0x44, 0x33, 0x22, 0x11,
                 0x41, 0x8f, 0x42, 0x55,
                 0x41, 0x8f, 0x82, 0x44, 0x33, 0x22, 0x11};
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_popx() {
  assemCtxPo ctx = createCtx();

  pop(IX(RAX, RDX, 4, 0x55), ctx); // pop 0x55(rax,r10*4)
  pop(IX(RDX, RAX, 4, 0x11223344), ctx); // pop 0x11223344(rdx,rax*4)
  pop(IX(RAX, R10, 4, 0x55), ctx); // pop 0x55(rax,r10*4)
  pop(IX(R12, R10, 4, 0x55), ctx); // pop 055(r12,r10*4)
  pop(IX(R12, RAX, 4, 0x55), ctx); // pop 0x55(r12,rax*4)

  uint8 tgt[] = {0x8f, 0x44, 0x90, 0x55,
                 0x8f, 0x84, 0x82, 0x44, 0x33, 0x22, 0x11,
                 0x42, 0x8f, 0x44, 0x90, 0x55,
                 0x43, 0x8f, 0x44, 0x94, 0x55,
                 0x41, 0x8f, 0x44, 0x84, 0x55,
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_pushri() {
  assemCtxPo ctx = createCtx();

  push(RG(RAX), ctx);
  push(RG(RSI), ctx);
  push(RG(R12), ctx);

  push(IM(0x55), ctx);
  push(IM(0x11223344), ctx);

  uint8 tgt[] = {0x50,
                 0x56,
                 0x41, 0x54,
                 0x6a, 0x55,
                 0x68, 0x44, 0x33, 0x22, 0x11};
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_pushb() {
  assemCtxPo ctx = createCtx();

  push(BS(RAX, 0x55), ctx);
  push(BS(RAX, 0x11223344), ctx);

  push(BS(R10, 0x55), ctx);
  push(BS(R10, 0x11223344), ctx);

  uint8 tgt[] = {0xff, 0x70, 0x55,
                 0xff, 0xb0, 0x44, 0x33, 0x22, 0x11,
                 0x41, 0xff, 0x72, 0x55,
                 0x41, 0xff, 0xb2, 0x44, 0x33, 0x22, 0x11};
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_pushx() {
  assemCtxPo ctx = createCtx();

  push(IX(RAX, RDX, 4, 0x55), ctx);
  push(IX(RAX, RDX, 4, 0x11223344), ctx);
  push(IX(RAX, R10, 4, 0x55), ctx);
  push(IX(R12, R10, 4, 0x55), ctx);
  push(IX(R12, RAX, 4, 0x55), ctx);

  uint8 tgt[] = {0xff, 0x74, 0x90, 0x55,
                 0xff, 0xb4, 0x90, 0x44, 0x33, 0x22, 0x11,
                 0x42, 0xff, 0x74, 0x90, 0x55,
                 0x43, 0xff, 0x74, 0x94, 0x55,
                 0x41, 0xff, 0x74, 0x84, 0x55,
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_setcc() {
  assemCtxPo ctx = createCtx();

  setne(RAX, ctx);
  setne(RCX, ctx);
  sets(RDX, ctx);

  uint8 tgt[] = {0x0f, 0x95, 0xc0,
                 0x0f, 0x95, 0xc1,
                 0x0f, 0x98, 0xc2};

  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_sbb() {
  assemCtxPo ctx = createCtx();

  sbb(RG(R10), RG(RAX), ctx);
  sbb(RG(RAX), RG(R10), ctx);
  sbb(RG(R10), RG(R9), ctx);
  sbb(RG(R9), RG(R10), ctx);

  sbb(RG(R10), IM(0x11223344), ctx);
  sbb(RG(RCX), IM(0x55667788), ctx);

  sbb(RG(R10), BS(RAX, 0x11223344), ctx);
  sbb(BS(RAX, 0x11223344), RG(R10), ctx);
  sbb(BS(RCX, 0x11223344), IM(0x55667788), ctx);
  sbb(BS(R10, 0x11223344), IM(0x55667788), ctx);
  sbb(BS(R9, 0x11223344), IM(0x55667788), ctx);
  sbb(BS(RAX, 0x11223344), IM(0x55667788), ctx);

  sbb(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  sbb(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
  sbb(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  uint8 tgt[] = {0x49, 0x19, 0xc2, // sbb r10,rax
                 0x4c, 0x19, 0xd0, // sbb rax,r10
                 0x4d, 0x19, 0xca, // sbb r10,r9
                 0x4d, 0x19, 0xd1, // sbb r9,r10
                 0x49, 0x81, 0xda, 0x44, 0x33, 0x22, 0x11, //         	sbbq	%r10,$287454020
                 0x48, 0x81, 0xd9, 0x88, 0x77, 0x66, 0x55, //         	sbbq	%rcx,$1432778632,
                 0x4c, 0x1b, 0x90, 0x44, 0x33, 0x22, 0x11, //         	sbbq	%r10,287454020(%rax)
                 0x4c, 0x19, 0x90, 0x44, 0x33, 0x22, 0x11, //        	sbbq 287454020(%rax),%r10
                 0x48, 0x81, 0x99, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // sbbq	287454020(%rcx),$1432778632
                 0x49, 0x81, 0x9a, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // sbbq	287454020(%r10),$1432778632
                 0x49, 0x81, 0x99, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // sbbq	287454020(%r9),$1432778632
                 0x48, 0x81, 0x98, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // sbbq	287454020(%rax),$1432778632
                 0x4e, 0x1b, 0x54, 0xd2, 0x55, //               	sbbq	%rcx,85(%rdx,%r10,8)
                 0x4e, 0x19, 0x54, 0xd2, 0x55,  //             	sbbq	%r10, 85(%rdx,%r10,8)
                 0x4a, 0x81, 0x5c, 0xd2, 0x55, 0x44, 0x33, 0x22, 0x11, //  	sbbq 85(%rdx,%r10,8),$287454020
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_sub() {
  assemCtxPo ctx = createCtx();

  sub(RG(R10), RG(RAX), ctx);
  sub(RG(RAX), RG(R10), ctx);
  sub(RG(R10), RG(R9), ctx);
  sub(RG(R9), RG(R10), ctx);

  sub(RG(R10), IM(0x11223344), ctx);
  sub(RG(RCX), IM(0x55667788), ctx);

  sub(RG(R10), BS(RAX, 0x11223344), ctx);
  sub(BS(RAX, 0x11223344), RG(R10), ctx);
  sub(BS(RCX, 0x11223344), IM(0x55667788), ctx);
  sub(BS(R10, 0x11223344), IM(0x55667788), ctx);
  sub(BS(R9, 0x11223344), IM(0x55667788), ctx);
  sub(BS(RAX, 0x11223344), IM(0x55667788), ctx);

  sub(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  sub(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
  sub(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  uint8 tgt[] = {0x49, 0x29, 0xc2, // sub r10,rax
                 0x4c, 0x29, 0xd0, // sub rax,r10
                 0x4d, 0x29, 0xca, // sub r10,r9
                 0x4d, 0x29, 0xd1, // sub r9,r10
                 0x49, 0x81, 0xea, 0x44, 0x33, 0x22, 0x11, //         	subq	%r10,$287454020
                 0x48, 0x81, 0xe9, 0x88, 0x77, 0x66, 0x55, //         	subq	%rcx,$1432778632,
                 0x4c, 0x2b, 0x90, 0x44, 0x33, 0x22, 0x11, //         	subq	%r10,287454020(%rax)
                 0x4c, 0x29, 0x90, 0x44, 0x33, 0x22, 0x11, //        	subq 287454020(%rax),%r10
                 0x48, 0x81, 0xa9, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // subq	287454020(%rcx),$1432778632
                 0x49, 0x81, 0xaa, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // subq	287454020(%r10),$1432778632
                 0x49, 0x81, 0xa9, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // subq	287454020(%r9),$1432778632
                 0x48, 0x81, 0xa8, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // subq	287454020(%rax),$1432778632
                 0x4e, 0x2b, 0x54, 0xd2, 0x55, //               	subq	%rcx,85(%rdx,%r10,8)
                 0x4e, 0x29, 0x54, 0xd2, 0x55,  //             	subq	%r10, 85(%rdx,%r10,8)
                 0x4a, 0x81, 0x6c, 0xd2, 0x55, 0x44, 0x33, 0x22, 0x11, //  	subq 85(%rdx,%r10,8),$287454020
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_testr() {
  assemCtxPo ctx = createCtx();

  test(RG(RAX), RG(RAX), ctx);
  test(RG(RCX), RG(RAX), ctx);
  test(RG(RDX), RG(RCX), ctx);
  test(RG(R10), RG(RAX), ctx);
  test(RG(RAX), RG(R10), ctx);
  test(RG(R12), RG(R10), ctx);

  uint8 tgt[] = {0x48, 0x85, 0xc0,
                 0x48, 0x85, 0xc1,
                 0x48, 0x85, 0xca,
                 0x49, 0x85, 0xc2,
                 0x4c, 0x85, 0xd0,
                 0x4d, 0x85, 0xd4
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_testrm() {
  assemCtxPo ctx = createCtx();

  test(RG(RAX), BS(RAX, 0x11223344), ctx);
  test(RG(RCX), BS(RAX, 0x11223344), ctx);
  test(RG(RDX), IX(RCX, RAX, 4, 0x55), ctx);
  test(RG(R10), IX(RCX, RAX, 4, 0x55), ctx);
  test(RG(RDX), IX(RCX, R10, 4, 0x55), ctx);

  test(RG(RAX), IM(0x11223344), ctx);
  test(RG(RDX), IM(0x55667788), ctx);
  test(RG(R10), IM(0x55667788), ctx);

  uint8 tgt[] = {0x48, 0x85, 0x80, 0x44, 0x33, 0x22, 0x11,
                 0x48, 0x85, 0x88, 0x44, 0x33, 0x22, 0x11,
                 0x48, 0x85, 0x54, 0x81, 0x55,
                 0x4c, 0x85, 0x54, 0x81, 0x55,
                 0x4a, 0x85, 0x54, 0x91, 0x55,
                 0x48, 0xa9, 0x44, 0x33, 0x22, 0x11,
                 0x48, 0xf7, 0xc2, 0x88, 0x77, 0x66, 0x55,
                 0x49, 0xf7, 0xc2, 0x88, 0x77, 0x66, 0x55
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_testmr() {
  assemCtxPo ctx = createCtx();

  test(BS(RAX, 0x11223344), RG(RAX), ctx);
  test(BS(RAX, 0x11223344), RG(RCX), ctx);
  test(IX(RCX, RAX, 4, 0x55), RG(RDX), ctx);
  test(IX(RCX, RAX, 4, 0x55), RG(R10), ctx);
  test(IX(RCX, R10, 4, 0x55), RG(RDX), ctx);

  test(IM(0x11223344), RG(RAX), ctx);
  test(IM(0x55667788), RG(R10), ctx);

  uint8 tgt[] = {0x48, 0x85, 0x80, 0x44, 0x33, 0x22, 0x11, // testq	%rax, 287454020(%rax)
                 0x48, 0x85, 0x88, 0x44, 0x33, 0x22, 0x11,
                 0x48, 0x85, 0x54, 0x81, 0x55,
                 0x4c, 0x85, 0x54, 0x81, 0x55,
                 0x4a, 0x85, 0x54, 0x91, 0x55,
                 0x48, 0xa9, 0x44, 0x33, 0x22, 0x11,
                 0x49, 0xf7, 0xc2, 0x88, 0x77, 0x66, 0x55
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_xchg() {
  assemCtxPo ctx = createCtx();

  xchg(RG(R10), RG(RAX), ctx);
  xchg(RG(RAX), RG(R10), ctx);
  xchg(RG(R10), RG(R9), ctx);
  xchg(RG(R9), RG(R10), ctx);

  xchg(RG(R10), BS(RAX, 0x11223344), ctx);
  xchg(BS(RAX, 0x11223344), RG(R10), ctx);

  xchg(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  xchg(IX(RDX, R10, 8, 0x55), RG(R10), ctx);

  uint8 tgt[] = {0x49, 0x92,
                 0x49, 0x92,
                 0x4d, 0x87, 0xca,
                 0x4d, 0x87, 0xd1,
                 0x4c, 0x87, 0x90, 0x44, 0x33, 0x22, 0x11,
                 0x4c, 0x87, 0x90, 0x44, 0x33, 0x22, 0x11,
                 0x4e, 0x87, 0x54, 0xd2, 0x55,
                 0x4e, 0x87, 0x54, 0xd2, 0x55,
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

static retCode test_xor() {
  assemCtxPo ctx = createCtx();

  xor(RG(R10), RG(RAX), ctx);
  xor(RG(RAX), RG(R10), ctx);
  xor(RG(R10), RG(R9), ctx);
  xor(RG(R9), RG(R10), ctx);

  xor(RG(R10), IM(0x11223344), ctx);
  xor(RG(RCX), IM(0x55667788), ctx);

  xor(RG(R10), BS(RAX, 0x11223344), ctx);
  xor(BS(RAX, 0x11223344), RG(R10), ctx);
  xor(BS(RCX, 0x11223344), IM(0x55667788), ctx);
  xor(BS(R10, 0x11223344), IM(0x55667788), ctx);
  xor(BS(R9, 0x11223344), IM(0x55667788), ctx);
  xor(BS(RAX, 0x11223344), IM(0x55667788), ctx);

  xor(RG(R10), IX(RDX, R10, 8, 0x55), ctx);
  xor(IX(RDX, R10, 8, 0x55), RG(R10), ctx);
  xor(IX(RDX, R10, 8, 0x55), IM(0x11223344), ctx);

  uint8 tgt[] = {0x49, 0x31, 0xc2, // xor r10,rax
                 0x4c, 0x31, 0xd0, // xor rax,r10
                 0x4d, 0x31, 0xca, // xor r10,r9
                 0x4d, 0x31, 0xd1, // xor r9,r10
                 0x49, 0x81, 0xf2, 0x44, 0x33, 0x22, 0x11, //         	xorq	%r10,$287454020
                 0x48, 0x81, 0xf1, 0x88, 0x77, 0x66, 0x55, //         	xorq	%rcx,$1432778632,
                 0x4c, 0x33, 0x90, 0x44, 0x33, 0x22, 0x11, //         	xorq	%r10,287454020(%rax)
                 0x4c, 0x31, 0x90, 0x44, 0x33, 0x22, 0x11, //        	xorq 287454020(%rax),%r10
                 0x48, 0x81, 0xb1, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // xorq	287454020(%rcx),$1432778632
                 0x49, 0x81, 0xb2, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // xorq	287454020(%r10),$1432778632
                 0x49, 0x81, 0xb1, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // xorq	287454020(%r9),$1432778632
                 0x48, 0x81, 0xb0, 0x44, 0x33, 0x22, 0x11, 0x88, 0x77, 0x66, 0x55,   // xorq	287454020(%rax),$1432778632
                 0x4e, 0x33, 0x54, 0xd2, 0x55, //               	xorq	%rcx,85(%rdx,%r10,8)
                 0x4e, 0x31, 0x54, 0xd2, 0x55,  //             	xorq	%r10, 85(%rdx,%r10,8)
                 0x4a, 0x81, 0x74, 0xd2, 0x55, 0x44, 0x33, 0x22, 0x11, //  	xorq 85(%rdx,%r10,8),$287454020
  };
  return checkCode(tgt, NumberOf(tgt), ctx);
}

typedef int64 (*un_i64)(int64 x);
typedef int64 (*bin_i64)(int64 x, int64 y);

retCode test_addFun() {
  assemCtxPo ctx = createCtx();

  preamble(ctx, 0);
  mov(RG(RAX), RG(RDI), ctx);
  add(RG(RAX), RG(RSI), ctx);
  postamble(ctx);

  bin_i64 fn = (bin_i64) createCode(ctx);
  int64 reslt = fn(3, 5);
  return checkReslt(reslt, 8, "addFn");
}

retCode test_factFun() {
  assemCtxPo ctx = createCtx();

  codeLblPo fct = preamble(ctx, 0);
  codeLblPo l0 = defineLabel(ctx, "nonZero", undefinedPc);
  codeLblPo lx = defineLabel(ctx, "exit", undefinedPc);
  cmp(RG(RDI), IM(1), ctx);
  jg(l0, ctx);
  mov(RG(RAX), IM(1), ctx);
  jmp(LB(lx), ctx);
  setLabel(ctx, l0);
  push(RG(RDI), ctx);
  dec(RG(RDI), ctx);
  call(LB(fct), ctx);
  pop(RG(RDI), ctx);
  imul(RAX, RG(RDI), ctx);
  setLabel(ctx, lx);
  postamble(ctx);

  un_i64 fn = (un_i64) createCode(ctx);
  tryRet(checkReslt(fn(3), 6, "fact(3)"));
  tryRet(checkReslt(fn(10), 3628800, "fact(10)"));
  return Ok;
}

retCode checkCode(uint8 *src, integer srcLen, assemCtxPo ctx) {
  retCode ret;
  if (ctx->pc != srcLen) {
    logMsg(logFile, "%d bytes expected, %d bytes generated", srcLen, ctx->pc);
    logMsg(logFile, "actual bytes: %.*X", ctx->pc, ctx->bytes);
    ret = Error;
  } else
    ret = cmpBytes(src, ctx->bytes, srcLen);
  discardCtx(ctx);
  return ret;
}

retCode all_tests() {
  tests_run = 0;

  tryRet(run_test(test_adc));
  tryRet(run_test(test_add));
  tryRet(run_test(test_and));
  tryRet(run_test(test_call));
  tryRet(run_test(test_cmp));
  tryRet(run_test(test_dec));
  tryRet(run_test(test_idiv));
  tryRet(run_test(test_imul));
  tryRet(run_test(test_inc));
  tryRet(run_test(test_jcc));
  tryRet(run_test(test_jmp));
  tryRet(run_test(test_lea));
  tryRet(run_test(test_lbl_lea));
  tryRet(run_test(test_mov));
  tryRet(run_test(test_movsx));
  tryRet(run_test(test_neg));
  tryRet(run_test(test_not));
  tryRet(run_test(test_or));
  tryRet(run_test(test_popr));
  tryRet(run_test(test_popb));
  tryRet(run_test(test_popx));
  tryRet(run_test(test_pushri));
  tryRet(run_test(test_pushb));
  tryRet(run_test(test_pushx));
  tryRet(run_test(test_ret));
  tryRet(run_test(test_setcc));
  tryRet(run_test(test_sbb));
  tryRet(run_test(test_sub));
  tryRet(run_test(test_testr));
  tryRet(run_test(test_testrm));
  tryRet(run_test(test_testmr));
  tryRet(run_test(test_xchg));
  tryRet(run_test(test_xor));

  tryRet(run_test(test_addFun));
  tryRet(run_test(test_factFun));

  return Ok;
}
