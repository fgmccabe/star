//
// Created by Francis McCabe on 6/25/25.
//
#include <errorCodes.h>
#include "arith.h"
#include "escapeP.h"
#include "engineP.h"
#include "globals.h"

ReturnStatus g__abort(enginePo P) {
  struct term_record *lc = popVal(P);
  struct term_record *msg = popVal(P);
  abort_star(P, lc, msg);

  pshVal(P, unitEnum);
  return Normal;
}

void abort_star(enginePo P, termPo lc, termPo msg) {
  logMsg(logFile, "Abort %T at %L", msg, lc);
  verifyProc(P, processHeap(P));
  stackTrace(P, logFile, P->stk, displayDepth, showPrognames, -1);
  star_exit(99);
}

ReturnStatus g__stackTrace(enginePo P) {
  strBufferPo str = newStringBuffer();

  stackTrace(P, O_IO(str), P->stk, displayDepth, showArguments, -1);

  pshVal(P, allocateFromStrBuffer(processHeap(P), str));
  closeIo(O_IO(str));

  return Normal;
}

ReturnStatus g__gc(enginePo P){
  integer amnt = integerVal(popVal(P));
  retCode ret = gcCollect(processHeap(P),amnt);
  if(ret==Ok) {
    pshVal(P,unitEnum);
    return Normal;
  } else{
    pshVal(P,eFAIL);
    return Abnormal;
  }
}
