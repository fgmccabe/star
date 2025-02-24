//
// Created by Francis McCabe on 2/21/25.
//

#ifndef STAR_ASMINFRA_H
#define STAR_ASMINFRA_H

#include <config.h>
#include "array.h"

typedef struct assem_ctx {
  unsigned char *bytes;
  uint32 size;
  uint32 pc;
  arrayPo lbls;
} AssemCtxRecord, *assemCtxPo;

typedef struct assem_lbl {
  arrayPo refs;
  integer pc;
} AssemLblRecord, *codeLblPo;

void initAssem();
assemCtxPo createCtx();
void discardCtx(assemCtxPo ctx);

extern integer undefinedPc;
uint32 currentPc(assemCtxPo ctx);

typedef void (*lblRefUpdater)(assemCtxPo ctx, codeLblPo lbl, integer pc);
retCode addLabelReference(assemCtxPo ctx, codeLblPo lbl, integer pc, lblRefUpdater updater);

void emitU8(assemCtxPo ctx, uint8 byte);
void emitU16(assemCtxPo ctx, uint16 word);
void emitU32(assemCtxPo ctx, uint32 word);
void emitU64(assemCtxPo ctx, uint64 word);
void updateU32(assemCtxPo ctx, integer pc, uint32 word);
uint32 readCtxAtPc(assemCtxPo ctx, integer pc);


#endif //STAR_ASMINFRA_H
