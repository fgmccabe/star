//
// Created by Francis McCabe on 7/1/20.
//

#include "config.h"
#include <turm.h>
#include "utils.h"
#include "codeP.h"
#include "lower.h"
#include "jitP.h"
#include "jitOps.h"

#include "stack.h"

integer jitThreshold = 1000;

#ifdef TRACEJIT
logical traceJit = False;
#endif

#undef instruction
#define instruction(Op, A1, A2, Dl, Cmt)    \
    case Op:          \
      ret = jit_##Op(code,&pc,context); \
      break;

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen) {
  insPo code = entryPoint(mtd);
  int len = insCount(mtd);
  integer pc = 0;
  jitCompPo context = jitContext(mtd);

  retCode ret = jit_preamble(mtd, context);

  while (ret == Ok && pc < len) {
    switch (code[pc++]) {
#include "instructions.h"

      default:
        return Error;
    }
  }

  if (ret == Ok)
    ret = jit_postamble(mtd, context);

  return ret;
}

termPo invokeJitMethod(methodPo mtd, heapPo H, stackPo stk) {
  switch (codeArity(mtd)) {
    case 0:
      return codeJit(mtd)();
    case 1:
      return ((jitCode1) codeJit(mtd))(topStack(stk));
    case 2:
      return ((jitCode2) codeJit(mtd))(topStack(stk), peekStack(stk, 1));
    case 3:
      return ((jitCode3) codeJit(mtd))(topStack(stk), peekStack(stk, 1), peekStack(stk, 2));
    case 4:
      return ((jitCode4) codeJit(mtd))(topStack(stk), peekStack(stk, 1), peekStack(stk, 2), peekStack(stk, 3));
    default: {
      integer arity = codeArity(mtd);
      termPo args[arity];
      for (integer ix = 0; ix < arity; ix++) {
        args[ix] = peekStack(stk, ix);
      }
      return ((jitCodeStar) codeJit(mtd))(args);
    }
  }
}
