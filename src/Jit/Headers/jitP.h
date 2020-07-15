//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JITP_H
#define STAR_JITP_H

#include "jit.h"

#define MAX_VSTACK 256

typedef struct jit_label_ *jitLblPo;

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
} vOperand;


typedef struct jit_compiler_ {
  void *codeBase;
  integer bufferLen;
  integer pc;
  methodPo mtd;
  vOperand vStack[MAX_VSTACK];
  integer vTop;
}JitCompiler;

jitCompPo jitContext(methodPo mtd);

#endif //STAR_JITP_H
