//
// Created by Francis McCabe on 2/12/23.
//

#include "jit.h"
#include <string.h>
#include <errno.h>
#include <sys/mman.h>
#include "jitP.h"
#include <stdlib.h>
#include <assert.h>

#include "formioP.h"

#ifdef TRACEJIT
tracingLevel traceAssem = noTracing;
#endif

static poolPo lblPool = Null;
static poolPo asmPool = Null;

integer undefinedPc = -1;

typedef struct lbl_ref {
  lblRefUpdater updater;
  integer pc;
} AssemLblRefRecord;

static retCode showFlexOp(ioPo f, void *data, long depth, long precision, logical alt) {
  FlexOp *flex = (FlexOp *) data;
  switch (flex->mode) {
    case imm: // Immediate value
      return outMsg(f, "#%d", flex->immediate);
    case shft: {
      switch (flex->shift) {
        case LSL:
          return outMsg(f, "%R lsl #%d", flex->reg, flex->immediate);
        case LSR:
          return outMsg(f, "%R lsr #%d", flex->reg, flex->immediate);
        case ASR:
          return outMsg(f, "%R asr #%d", flex->reg, flex->immediate);
        case ROR:
          return outMsg(f, "%R ror #%d", flex->reg, flex->immediate);
      }
    }
    case reg: // register
      return outMsg(f, "X%d", flex->reg);
    case fp: // floating point register
      return outMsg(f, "F%d", flex->reg);
    case extnd: {
      switch (flex->ext) {
        case U_XTB:
          return outMsg(f, "[%R, %R, uxtb]", flex->reg, flex->rgm);
        case U_XTH:
          return outMsg(f, "[%R, %R, uxth]", flex->reg, flex->rgm);
        case U_XTW:
          return outMsg(f, "[%R, %R, uxtw]", flex->reg, flex->rgm);
        case U_XTX:
          return outMsg(f, "[%R, %R, lsl]", flex->reg, flex->rgm);
        case S_XTB:
          return outMsg(f, "[%R, %R, sxtb]", flex->reg, flex->rgm);
        case S_XTH:
          return outMsg(f, "[%R, %R, sxth]", flex->reg, flex->rgm);
        case S_XTW:
          return outMsg(f, "[%R, %R, sxtw]", flex->reg, flex->rgm);
        case S_XTX:
          return outMsg(f, "[%R, %R, sxtx]", flex->reg, flex->rgm);
      }
      return outMsg(f, "[%R, %R, unknown]", flex->reg, flex->rgm);
    }
    case postX: // post increment
      return outMsg(f, "[%R], #%d", flex->reg, flex->immediate);
    case preX: // predecrement
      return outMsg(f, "[%R, #%d]!", flex->reg, flex->immediate);
    case sOff: // signed offset
      return outMsg(f, "X%d[%d]", flex->reg, flex->immediate);
    case pcRel: // relative to PC
      return outMsg(f, "[pc, #%x]", flex->immediate);
    default:
      return outMsg(f, "unknown addressing mode");
  }
}

static retCode showAssemLbl(ioPo f, void *data, long depth, long precision, logical alt) {
  codeLblPo lbl = (codeLblPo) data;
  if (isLabelDefined(lbl)) {
    return outMsg(f, "@%x", lbl->pc);
  }
  return outMsg(f, "unknown label");
}

retCode showArmReg(ioPo f, void *data, long depth, long precision, logical alt) {
  int reg = (int) (long) data;
  if (alt)
    return outMsg(f, "D%d", reg);
  else
    return outMsg(f, "X%d", reg);
}

void initAssem() {
  if (asmPool == Null) {
    lblPool = newPool(sizeof(AssemLblRecord), 128);
    asmPool = newPool(sizeof(AssemCtxRecord), 128);

#ifdef TRACEJIT
    if (traceAssem > noTracing || traceJit > noTracing) {
      installMsgProc('F', showFlexOp);
      installMsgProc('R', showArmReg);
      installMsgProc('X', showAssemLbl);
    }
#endif
  }
}

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

uint32 currentPc(assemCtxPo ctx) {
  return ctx->pc;
}

retCode clearLbl(codeLblPo lbl) {
  check(isLabelDefined(lbl), "label not defined");
  if (lbl->refs != Null) {
    eraseArray(lbl->refs, NULL, NULL);
  }
  freePool(lblPool, lbl);
  return Ok;
}

retCode clrLblProc(void *l, integer ix, void *cl) {
  assemCtxPo ctx = (assemCtxPo) cl;
  return clearLbl((codeLblPo) l);
}

retCode cleanupLabels(assemCtxPo ctx) {
  if (ctx->lbls != Null) {
    ctx->lbls = eraseArray(ctx->lbls, clrLblProc, ctx);
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

codeLblPo here_(assemCtxPo ctx) {
  return defineLabel(ctx, ctx->pc);
}

codeLblPo defineLabel(assemCtxPo ctx, integer pc) {
  codeLblPo lbl = (codeLblPo) allocPool(lblPool);
  lbl->refs = Null;
  lbl->pc = pc;

  return lbl;
}

codeLblPo newLabel(assemCtxPo ctx) {
  codeLblPo lbl = (codeLblPo) allocPool(lblPool);
  lbl->refs = Null;
  lbl->pc = undefinedPc;

  return lbl;
}

codeLblPo setLabel_(assemCtxPo ctx, codeLblPo lbl) {
  lbl->pc = ctx->pc;
  ClInfo info = {.ctx = ctx, .lbl = lbl};
  if (lbl->refs != Null) {
    processArrayElements(lbl->refs, updateLblEntry, &info);
    lbl->refs = eraseArray(lbl->refs, NULL, NULL);
  }
  return lbl;
}

retCode addLabelReference(assemCtxPo ctx, codeLblPo lbl, integer pc, lblRefUpdater updater) {
  check(!isLabelDefined(lbl), "label should not be defined");
  if (lbl->refs == Null) {
    lbl->refs = allocArray(sizeof(AssemLblRefRecord), 4, True);
  }
  AssemLblRefRecord ref = {.updater = updater, .pc = pc};
  return appendEntry(lbl->refs, &ref);
}

logical isLabelDefined(codeLblPo lbl) {
  return lbl->pc != undefinedPc;
}

uint64 labelTgt(codeLblPo lbl) {
  assert(isLabelDefined(lbl));
  return lbl->pc;
}

void labelConst(codeLblPo lbl, assemCtxPo ctx) {
  emitU64(ctx, labelTgt(lbl));
}

#define UNDEF_LBL_LANDING_PAD 0x12244

void labelDisp32(assemCtxPo ctx, codeLblPo lbl, integer pc) {
  check(readCtxAtPc(ctx, pc) == UNDEF_LBL_LANDING_PAD, "bad label reference");

  uint32 delta = (uint32) ((integer) labelTgt(lbl) - (pc + PLATFORM_PC_DELTA));
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
    emitU32(ctx, (uint32) delta);
  } else {
    addLabelReference(ctx, tgt, ctx->pc, labelDisp32);
    emitU32(ctx, UNDEF_LBL_LANDING_PAD);
  }
}

void emitU8(assemCtxPo ctx, uint8 byte) {
  if (ctx->pc + 1 >= ctx->size) {
    uint32 newSize = (ctx->size * 3) / 2;
    void *newBuffer = realloc(ctx->bytes, newSize);
    if (newBuffer != Null) {
      ctx->bytes = newBuffer;
      ctx->size = newSize;
    } else {
      syserr("Could not allocate buffer for code generation");
    }
  }
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

jittedCode createCode(assemCtxPo ctx) {
  cleanupLabels(ctx);
  extern int errno;
  errno = 0;
  void *code = mmap(Null, ctx->pc, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);

  switch (errno) {
    case 0:
      break;
    default:
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
  return (jittedCode) code;
}
