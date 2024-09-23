//
// Created by Francis McCabe on 2/12/23.
//

#include "jit.h"
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include <lower.h>
#include "jitP.h"
#include "pool.h"
#include <stdlib.h>
#include <assert.h>
#include "array.h"

static poolPo lblPool = Null;
static poolPo asmPool = Null;

integer undefinedPc = -1;

void initAssem() {
  if (asmPool == Null) {
    lblPool = newPool(sizeof(AssemLblRecord), 128);
    asmPool = newPool(sizeof(AssemCtxRecord), 128);
  }
}

static retCode clearLbl(codeLblPo lbl);

assemCtxPo createCtx() {
  initAssem();

  assemCtxPo ctx = (assemCtxPo) allocPool(asmPool);
  ctx->bytes = malloc(1024);
  ctx->size = 1024;
  ctx->pc = 0;
  ctx->lbls = allocArray(sizeof(AssemLblRecord), 16, True);

  clearCodeCtxMaps(ctx);
  return ctx;
}

void discardCtx(assemCtxPo ctx) {
  cleanupLabels(ctx);
  if (ctx->bytes != Null) {
    free(ctx->bytes);
    ctx->bytes = Null;
  }
  freePool(asmPool, ctx);
}

uint32 currentPc(assemCtxPo ctx){
  return ctx->pc;
}

void verifyJitCtx(jitCompPo jitCtx, integer amnt, integer space) {
  check(jitCtx->vTop >= amnt && jitCtx->vTop < NumberOf(jitCtx->vStack) - space, "stack out of bounds");
}

retCode clearLbl(codeLblPo lbl) {
  check(lbl->refs == Null, "label not defined");
  if (lbl->refs != Null) {
    eraseArray(lbl->refs, NULL, NULL);
  }
  freePool(lblPool, lbl);
  return Ok;
}

retCode clrLblProc(void *l, integer ix, void *cl) {
  return clearLbl((codeLblPo) l);
}

retCode cleanupLabels(assemCtxPo ctx) {
  if (ctx->lbls != Null) {
    eraseArray(ctx->lbls, clrLblProc, NULL);
    ctx->lbls = Null;
  }
  return Ok;
}

typedef struct {
  assemCtxPo ctx;
  codeLblPo lbl;
} ClInfo;

static retCode updateLblEntry(void *entry, integer ix, void *cl) {
  AssemLblRefRecord *refEntry = (AssemLblRefRecord *) entry;
  ClInfo *info = (ClInfo *) cl;
  refEntry->updater(info->ctx, info->lbl, refEntry->pc);
  return Ok;
}

codeLblPo defineLabel(assemCtxPo ctx, integer pc) {
  codeLblPo lbl = (codeLblPo) allocPool(lblPool);
  lbl->refs = Null;
  lbl->pc = pc;

  return lbl;
}

codeLblPo newLabel(assemCtxPo ctx){
  codeLblPo lbl = (codeLblPo) allocPool(lblPool);
  lbl->refs = Null;
  lbl->pc = -1;

  return lbl;
}

void setLabel(assemCtxPo ctx, codeLblPo lbl) {
  lbl->pc = ctx->pc;
  ClInfo info = {.ctx=ctx, .lbl=lbl};
  if (lbl->refs != Null) {
    processArrayElements(lbl->refs, updateLblEntry, &info);
    lbl->refs = eraseArray(lbl->refs, NULL, NULL);
  }
}

retCode addLabelReference(assemCtxPo ctx, codeLblPo lbl, integer pc, lblRefUpdater updater) {
  check(!isLabelDefined(lbl), "label should not be defined");
  if (lbl->refs == Null) {
    lbl->refs = allocArray(sizeof(AssemLblRefRecord), 4, True);
  }
  AssemLblRefRecord ref = {.updater=updater, .pc=pc};
  return appendEntry(lbl->refs, &ref);
}

logical isLabelDefined(codeLblPo lbl) {
  return lbl->pc != undefinedPc;
}

uint64 labelTgt(codeLblPo lbl) {
  assert(isLabelDefined(lbl));
  return lbl->pc;
}

#define UNDEF_LBL_LANDING_PAD 0x12244

void labelDisp32(assemCtxPo ctx, codeLblPo lbl, integer pc) {
  check(readCtxAtPc(ctx, pc) == UNDEF_LBL_LANDING_PAD, "bad label reference");

  uint32 delta = (uint32)((integer) labelTgt(lbl) - (pc + PLATFORM_PC_DELTA));
  if (isI32(delta)) {
    updateU32(ctx, pc, delta);
  } else {
    check(False, "adjusted displacement too large");
  }
}

integer lblDeltaRef(assemCtxPo ctx, codeLblPo tgt) {
  if (isLabelDefined(tgt))
    return (integer) labelTgt(tgt) - (ctx->pc + PLATFORM_PC_DELTA);
  else
    return UNDEF_LBL_LANDING_PAD;
}

void emitLblRef(assemCtxPo ctx, codeLblPo tgt) {
  if (isLabelDefined(tgt)) {
    integer delta = (integer) labelTgt(tgt) - (ctx->pc + PLATFORM_PC_DELTA);
    if (!isI32(delta)) {
      check(False, "label displacement too large");
      return;
    }
    emitU32(ctx, (uint32)delta);
  } else {
    addLabelReference(ctx, tgt, ctx->pc, labelDisp32);
    emitU32(ctx, UNDEF_LBL_LANDING_PAD);
  }
}

void emitU8(assemCtxPo ctx, uint8 byte) {
  assert(ctx->pc < ctx->size);
  ctx->bytes[ctx->pc++] = byte;
}

void emitU16(assemCtxPo ctx, uint16 word) {
  emitU8(ctx, word & 0xffu);
  emitU8(ctx, (uint8) (word >> 8u));
}

void emitU32(assemCtxPo ctx, uint32 word) {
  emitU16(ctx, word & 0xffffu);
  emitU16(ctx, (uint16) (word >> 16u));
}

void emitU64(assemCtxPo ctx, uint64 word) {
  emitU32(ctx, word & 0xffffffffu);
  emitU32(ctx, (uint32) (word >> 32u));
}

void updateU32(assemCtxPo ctx, integer pc, uint32 word) {
  ctx->bytes[pc++] = (word & 0xffu);
  ctx->bytes[pc++] = (word >> 8u) & 0xffu;
  ctx->bytes[pc++] = (word >> 16u) & 0xffu;
  ctx->bytes[pc] = (word >> 24u) & 0xffu;
}

uint32 readCtxAtPc(assemCtxPo ctx, integer pc) {
  check(pc >= 0 && pc <= ctx->pc, "pc out of bounds");
  return ((unsigned) ctx->bytes[pc]) | (unsigned) (ctx->bytes[pc + 1] << 8u) | (unsigned) (ctx->bytes[pc + 2] << 16u) |
         (unsigned) (ctx->bytes[pc + 3] << 24u);
}

logical isByte(int64 x) {
  return x >= -128 && x < 128;
}

#define MAX_I32 0x7fffffffl
#define MIN_I32 (-0x80000000l)

logical isI32(int64 x) {
  return x >= MIN_I32 && x <= MAX_I32;
}

jitCode createCode(assemCtxPo ctx) {
  cleanupLabels(ctx);
  extern int errno;
  errno = 0;
  void *code = mmap(Null, ctx->pc, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);

  switch (errno) {
    case 0:
      break;
    case EACCES:
      syserr("could not allocate JIT memory");

    case EBADF:
      syserr("The fd argument is not a valid open file descriptor");

    case EINVAL:
      syserr("flags includes bits that are not part of any valid flags value.");

    case ENXIO:
      syserr("Addresses in the specified range are invalid for fd.");

    case EOVERFLOW:
      syserr("Addresses in the specified range exceed the maximum offset set for fd.");
  }
  memcpy(code, ctx->bytes, ctx->pc);

  switch (mprotect(code, ctx->pc, PROT_READ | PROT_EXEC)) {
    case 0:
      break;
    case EACCES:
      syserr(
        "The requested protection conflicts with the access permissions of the process on the specified address range");
    case EINVAL:
      syserr("addr is not a multiple of the page size (i.e.  addr is not page-aligned).");
    case ENOMEM:
      syserr("The specified address range is outside of the address range of the process or includes an unmapped page");
    case ENOTSUP:
      syserr("The combination of accesses requested in prot is not supported.");
  }

  free(ctx->bytes);
  ctx->bytes = Null;
  return (jitCode)code;
}
