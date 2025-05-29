//
// Created by Francis McCabe on 10/11/21.
//

#ifndef STAR_LOWERP_H
#define STAR_LOWERP_H

#include "hash.h"
#include "lower.h"
#include "arm64P.h"
#include "macros.h"
#include "term.h"
#include "arith.h"
#include "char.h"

static inline logical isSmall(termPo x) {
  if (isInteger(x))
    return is16bit(integerVal(x));
  else if (isChar(x))
    return is16bit((integer) charVal(x));
  else
    return False;
}

typedef struct {
  FlexOp src;
} StackEntry;

typedef struct {
  StackEntry valueStack[128];
  int32 stackHeight;
} ValueStack, *valueStackPo;

typedef struct jitBlock_ *jitBlockPo;

typedef struct jitBlock_ {
  valueStackPo valStk;
  insPo code;
  int32 startPc;
  codeLblPo breakLbl;
  codeLblPo loopLbl;
  jitBlockPo parent;
} JitBlock;

#endif //STAR_LOWERP_H
