//
// Created by Francis McCabe on 6/29/20.
//

#include <labels.h>
#include "optionP.h"
#include "assert.h"

termPo noneEnum;
labelPo someCons;

void initOption() {
  noneEnum = declareEnum("none", 0);
  someCons = declareLbl("some", 1, 1);
}

normalPo wrapSome(termPo lhs) {
  int root = gcAddRoot(&lhs);
  normalPo tpl = allocateStruct(someCons);
  setArg(tpl, 0, lhs);
  gcReleaseRoot(root);
  return tpl;
}

logical isSome(termPo t) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return sameLabel(termLbl(c), someCons);
  }
  else
    return False;
}
