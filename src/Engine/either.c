//
// Created by Francis McCabe on 3/3/24.
//

#include <assert.h>
#include "eitherP.h"
#include "normalP.h"

labelPo eitherConstructor;
labelPo orConstructor;
termPo neitherEnum;

void initEither() {
  neitherEnum = declareEnum("star.either#neither", 1, globalHeap);

  eitherConstructor = declareLbl("star.either#either", 1, 0);
  orConstructor = declareLbl("star.either#other", 1, 2);
}

logical isNeither(termPo t) {
  return t == neitherEnum;
}

logical isEither(termPo t) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return (logical) (c->lbl == eitherConstructor);
  } else
    return False;
}

logical isOr(termPo t) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return (logical) (c->lbl == orConstructor);
  } else
    return False;
}

termPo eitherValue(termPo t) {
  assert(isEither(t));
  return nthElem(C_NORMAL(t), 0);
}

termPo orValue(termPo t) {
  assert(isOr(t));
  return nthElem(C_NORMAL(t), 0);
}

normalPo wrapEither(heapPo H, termPo t) {
  int root = gcAddRoot(H, &t);
  normalPo trm = allocateStruct(H, eitherConstructor);
  setArg(trm, 0, t);
  gcReleaseRoot(H, root);
  return trm;
}

normalPo wrapOr(heapPo H, termPo t) {
  int root = gcAddRoot(H, &t);
  normalPo trm = allocateStruct(H, orConstructor);
  setArg(trm, 0, t);
  gcReleaseRoot(H, root);
  return trm;
}
