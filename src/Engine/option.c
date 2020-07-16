//
// Created by Francis McCabe on 6/29/20.
//

#include <labels.h>
#include "optionP.h"
#include "assert.h"

labelPo noneEnum;
labelPo someCons;

void initOption() {
  noneEnum = declareLbl("star.core#none", 0);
  someCons = declareLbl("star.core#some", 1);
}

normalPo wrapSome(heapPo H, termPo lhs) {
  int root = gcAddRoot(H, &lhs);
  normalPo tpl = allocateStruct(H, someCons);
  setArg(tpl, 0, lhs);
  gcReleaseRoot(H, root);
  return tpl;
}

logical isSome(termPo t) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return (logical) (termLbl(c) == someCons);
  } else
    return False;
}

logical isNone(termPo t) {
  return t == (termPo) noneEnum;
}

termPo unwrapSome(normalPo p) {
  assert(isSome((termPo) p));
  return nthArg(p, 0);
}

