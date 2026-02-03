//
// Created by Francis McCabe on 6/25/25.
//
#include <errorCodes.h>
#include "arith.h"
#include "escapeP.h"
#include "engineP.h"
#include "globals.h"

ValueReturn s__abort(enginePo P, termPo lc, termPo msg) {
  abort_star(P, lc, msg);
  return normalReturn(unitEnum);
}

ReturnStatus g__abort(enginePo P) {
  termPo lc = popVal(P);
  termPo msg = popVal(P);
  ValueReturn ret = s__abort(P, lc, msg);
  pshVal(P, ret.value);
  return ret.status;
}

void abort_star(enginePo P, termPo lc, termPo msg) {
  char msgStr[MAX_SYMB_LEN];
  strMsg(msgStr,NumberOf(msgStr), "Abort %T at %L", msg, lc);
  verifyProc(P, processHeap(P));
  stackTrace(P, logFile, P->stk, displayDepth, showPrognames, MAX_INT32);
  syserr(msgStr);
}

ValueReturn s__stackTrace(enginePo P) {
  strBufferPo str = newStringBuffer();

  stackTrace(P, O_IO(str), P->stk, displayDepth, showArguments, MAX_INT32);

  termPo trace = allocateFromStrBuffer(processHeap(P), str);
  closeIo(O_IO(str));

  return normalReturn(trace);
}

ReturnStatus g__stackTrace(enginePo P) {
  ValueReturn ret = s__stackTrace(P);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__gc(enginePo P, termPo a) {
  integer amnt = integerVal(a);
  retCode ret = gcCollect(processHeap(P), amnt);
  if (ret == Ok) {
    return normalReturn(unitEnum);
  } else {
    return abnormalReturn(eFAIL);
  }
}

ReturnStatus g__gc(enginePo P) {
  termPo a = popVal(P);
  ValueReturn ret = s__gc(P, a);
  pshVal(P, ret.value);
  return ret.status;
}
