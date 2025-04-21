//
// Created by Francis McCabe on 4/21/25.
//

#include <assert.h>
#include "normalP.h"
#include "resultP.h"

labelPo normalResultConstructor;
labelPo abnormalConstructor;

void initResult() {
  normalResultConstructor = declareLbl("normal", 1, 1);
  abnormalConstructor = declareLbl("abnormal", 1, 0);
}

logical isResult(termPo t) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return (logical) (c->lbl == normalResultConstructor);
  } else
    return False;
}

logical isAbnormal(termPo t) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return (logical) (c->lbl == abnormalConstructor);
  } else
    return False;
}

termPo resultValue(termPo t) {
  assert(isResult(t));
  return nthElem(C_NORMAL(t), 0);
}

termPo abnormalValue(termPo t) {
  assert(isAbnormal(t));
  return nthElem(C_NORMAL(t), 0);
}

normalPo wrapResult(heapPo H, termPo t) {
  int root = gcAddRoot(H, &t);
  normalPo trm = allocateStruct(H, normalResultConstructor);
  setArg(trm, 0, t);
  gcReleaseRoot(H, root);
  return trm;
}

normalPo wrapAbnormal(heapPo H, termPo t) {
  int root = gcAddRoot(H, &t);
  normalPo trm = allocateStruct(H, abnormalConstructor);
  setArg(trm, 0, t);
  gcReleaseRoot(H, root);
  return trm;
}
