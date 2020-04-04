//
// Created by Francis McCabe on 4/1/20.
//

#ifndef STAR_JITP_H
#define STAR_JITP_H

#include "jit.h"

typedef struct _jit_compiler_ *jitCompPo;

typedef struct _jit_label_ *jitLblPo;

typedef enum{
  RX,
  R0,
  R1,
  R2,
  FP,
  SP
} JitRegister;

typedef struct _jit_compiler_ {
  void *codeBase;
  integer bufferLen;
  integer pc;
}JitCompiler;

#endif //STAR_JITP_H
