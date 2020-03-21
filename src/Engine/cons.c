//
// Created by Francis McCabe on 3/20/20.
//

#include <labels.h>
#include "consP.h"
#include "assert.h"

labelPo nilEnum;
labelPo consCons;

void initCons() {
  nilEnum = declareLbl("star.core#nl", 0);
  consCons = declareLbl("star.core#cns", 2);
}

normalPo allocateCons(heapPo H, termPo lhs, termPo rhs) {
  int root = gcAddRoot(H, &lhs);
  gcAddRoot(H, &rhs);
  normalPo tpl = allocateStruct(H, consCons);
  setArg(tpl, 0, lhs);
  setArg(tpl, 1, rhs);
  gcReleaseRoot(H, root);
  return tpl;
}

termPo consHead(normalPo p) {
  assert(p->lbl == consCons);
  return nthArg(p, 0);
}

termPo consTail(normalPo p) {
  assert(p->lbl == consCons);
  return nthArg(p, 1);
}

integer consLength(normalPo p) {
  integer ln = 0;
  termPo t = (termPo) p;
  while (isNormalPo(t) && termLbl(C_TERM(t)) == consCons) {
    t = nthArg(C_TERM(t), 1);
    ln++;
  }
  assert (t == (termPo)nilEnum);
  return ln;
}
