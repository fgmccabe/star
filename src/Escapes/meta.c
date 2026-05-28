//
// Created by Francis McCabe on 6/25/25.
//
#include <errorCodes.h>

#include "abort.h"
#include "arith.h"
#include "debug.h"
#include "escapeP.h"
#include "engineP.h"
#include "globals.h"

ValueReturn s__abort(enginePo P, termPo lc, termPo msg) {
  abort_star(P, lc, msg);
  return normalReturn(unitEnum);
}

void abort_star(enginePo P, termPo lc, termPo msg) {
  outMsg(logFile, "Abort %T at %L\n", msg);
  stackTrace(P, logFile, P->stk, displayDepth, showPrognames, 32);
  star_exit(P,abortCode);
}

ValueReturn s__stackTrace(enginePo P) {
  strBufferPo str = newStringBuffer();

  stackTrace(P, O_IO(str), P->stk, displayDepth, showArguments, MAX_INT32);

  termPo trace = allocateFromStrBuffer(processHeap(P), str);
  closeIo(O_IO(str));

  return normalReturn(trace);
}

ValueReturn s__gc(enginePo P, termPo a) {
  if (isDebugging()) {
    integer amnt = integerVal(a);
    retCode ret = gcCollect(processHeap(P), amnt);
    if (ret == Ok) {
      return normalReturn(unitEnum);
    }
    else {
      return abnormalReturn(eFAIL);
    }
  }
  else
    return abnormalReturn(eNOPERM);
}

ValueReturn s__break(enginePo P, termPo a) {
  outMsg(logFile, " User break: %T\n%_", a);
  return normalReturn(unitEnum);
}
