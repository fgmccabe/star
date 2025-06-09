//
// Created by Francis McCabe on 8/13/23.
//

#include "ooio.h"
#include "engine.h"
#include "future.h"
#include "arith.h"
#include "timer.h"
#include "globals.h"
#include "escape.h"

static void reportTimeout(void *cl);
static retCode checkTimeOut(futurePo ft, heapPo h, void *cl, void *cl2);

ReturnStatus g__settimeout(processPo P) {
  double delta = floatVal(popVal(P));

  futurePo ft = makeFuture(currentHeap, voidEnum, checkTimeOut, Null, Null);

  setTimer(delta, reportTimeout, (void *) ft);
  pshVal(P,unitEnum);
  return Normal;
}

void reportTimeout(void *cl) {
  futurePo fut = C_FUTURE(cl);
  resolveFuture(fut, unitEnum);
}

retCode checkTimeOut(futurePo ft, heapPo h, void *cl, void *cl2) {
  // Polling a timeout does nothing
  return Ok;
}
