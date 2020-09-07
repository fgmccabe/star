//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JITP_H
#define STAR_JITP_H

#include <config.h>
#include "jit.h"
#include "ooio.h"
#include "array.h"

#define MAX_VSTACK 256

typedef struct assem_ctx *codeCtxPo;
typedef struct assem_lbl *codeLblPo;

typedef unsigned char u8;
typedef signed char i8;

typedef int16 i16;
typedef uint16 u16;

typedef uint32 u32;
typedef int32 i32;

typedef int64 i64;
typedef uint64 u64;

typedef enum {
  int64Tp,
  fltTp,
  ptrTp
} lType;

typedef enum {
  argument,
  local,
  literal,
  immediate
} srcLoc;

typedef struct {
  lType type;
  srcLoc loc;
  termPo litrl;
  i32 ix;
} vOperand;

typedef struct jit_compiler_ {
  codeCtxPo codeBase;
  methodPo mtd;
  integer vTop;
  vOperand vStack[MAX_VSTACK];
} JitCompilerContext;

typedef struct assem_ctx {
  unsigned char *bytes;
  uint32 size;
  uint32 pc;
  hashPo lbls;
} AssemCtxRecord;

typedef struct assem_lbl {
  char nm[128];
  arrayPo refs;
  integer pc;
} AssemLblRecord;

void emitU8(codeCtxPo ctx, u8 byte);
void emitU16(codeCtxPo ctx, u16 word);
void emitU32(codeCtxPo ctx, u32 word);
void emitU64(codeCtxPo ctx, u64 word);
void updateU32(codeCtxPo ctx, integer pc, u32 word);
u32 readCtxAtPc(codeCtxPo ctx, integer pc);

jitCompPo jitContext(methodPo mtd);

void initAssemX64();
codeCtxPo createCtx();
void discardCtx(codeCtxPo ctx);
void *createCode(codeCtxPo ctx);

codeLblPo findLabel(codeCtxPo ctx, char *lName);
codeLblPo defineLabel(codeCtxPo ctx, char *lName, integer pc);
void setLabel(codeCtxPo ctx, codeLblPo lbl);
logical isLabelDefined(codeLblPo lbl);
retCode cleanupLabels(codeCtxPo ctx);

typedef void (*lblRefUpdater)(codeCtxPo ctx, codeLblPo lbl, integer pc);
retCode addLabelReference(codeCtxPo ctx, codeLblPo lbl, integer pc, lblRefUpdater updater);
static retCode updateLblEntry(void *entry, integer ix, void *cl);
void emitLblRef(codeCtxPo ctx, codeLblPo tgt);
void labelDisp32(codeCtxPo ctx, codeLblPo lbl, integer pc);

typedef struct lbl_ref {
  lblRefUpdater updater;
  integer pc;
} AssemLblRefRecord;

logical isByte(i64 x);
logical isI32(i64 x);


#endif //STAR_JITP_H
