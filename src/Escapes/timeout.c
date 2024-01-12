//
// Created by Francis McCabe on 8/13/23.
//

#include "timeout.h"

#include "ooio.h"
#include "engine.h"
#include "single.h"
#include "arith.h"
#include "timer.h"
#include "globals.h"

static void reportTimeout(void *cl);

ReturnStatus g__settimeout(heapPo h, termPo a1, termPo a2) {
  double delta = floatVal(a1);
  singlePo single = makeSingle(h);
  setTimer(delta, reportTimeout, (void *) single);
  return (ReturnStatus) {.ret=Ok, .result=unitEnum};
}

void reportTimeout(void *cl) {
  singlePo fut = (singlePo) cl;
  setSingle(fut, trueEnum);
}
