//
// Created by Francis McCabe on 7/1/20.
//

#include "config.h"
#include <turm.h>
#include "utils.h"
#include "codeP.h"
#include "jitP.h"
#include "jitOps.h"

#undef instruction
#define instruction(Op, A1, Dl, Cmt)    \
    case Op:          \
      ret = jit_##Op##Instruction(code,&pc,A1,context); \
      break;

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen) {
  insPo code = entryPoint(mtd);
  int len = insCount(mtd);
  integer pc = 0;
  retCode ret = Ok;
  jitCompPo context = jitContext(mtd);

  while (ret == Ok && pc < len) {
    switch (code[pc++]) {
#include "instructions.h"

      default:
        return Error;
    }
  }
  return Error;
}

