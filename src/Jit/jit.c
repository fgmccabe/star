//
// Created by Francis McCabe on 7/1/20.
//

#include "config.h"
#include <turm.h>
#include "utils.h"
#include "codeP.h"
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

  if(ret==Ok)

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
    case 5:
      return ((jitCode5) codeJit(mtd))(topStack(stk), peekStack(stk, 1), peekStack(stk, 2), peekStack(stk, 3),
                                       peekStack(stk, 4));
    case 6:
      return ((jitCode6) codeJit(mtd))(topStack(stk), peekStack(stk, 1), peekStack(stk, 2), peekStack(stk, 3),
                                       peekStack(stk, 4), peekStack(stk, 5));
    case 7:
      return ((jitCode7) codeJit(mtd))(topStack(stk), peekStack(stk, 1), peekStack(stk, 2), peekStack(stk, 3),
                                       peekStack(stk, 4), peekStack(stk, 5), peekStack(stk, 6));
    case 8:
      return ((jitCode8) codeJit(mtd))(topStack(stk), peekStack(stk, 1), peekStack(stk, 2), peekStack(stk, 3),
                                       peekStack(stk, 4), peekStack(stk, 5), peekStack(stk, 6), peekStack(stk, 7));
    default:
      syserr("cannot invoke jitted code");
      return Null;
  }
}
