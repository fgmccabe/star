//
// Created by Francis McCabe on 8/13/23.
//

#include "timeout.h"

#include "ooio.h"
#include "engine.h"
#include "future.h"
#include "arith.h"
#include "timer.h"
#include "globals.h"

static void reportTimeout(void *cl);
static retCode checkTimeOut(futurePo ft, heapPo h, void *cl, void *cl2);

ReturnStatus g__settimeout(heapPo h, termPo a1, termPo a2) {
  double delta = floatVal(a1);

  futurePo ft = makeFuture(h, voidEnum, checkTimeOut, Null, Null);

  setTimer(delta, reportTimeout, (void *) ft);
  return (ReturnStatus) {.ret=Normal, .result=unitEnum};
}

void reportTimeout(void *cl) {
  futurePo fut = C_FUTURE(cl);
  resolveFuture(fut, unitEnum);
}

retCode checkTimeOut(futurePo ft, heapPo h, void *cl, void *cl2) {
  // Polling a timeout does nothing
  return Ok;
}
