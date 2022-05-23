//
// Created by Francis McCabe on 3/20/20.
//

#include <labels.h>
#include <strings.h>
#include "consP.h"
#include "assert.h"

termPo nilEnum;
labelPo consCons;

void initCons() {
  nilEnum = declareEnum("star.core#nil", 1, globalHeap);
  consCons = declareLbl("star.core#cons", 2, 0);
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

logical isCons(termPo t) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return (logical) (c->lbl == consCons);
  } else
    return False;
}

logical isConsNil(termPo t) {
  return sameTerm(t, nilEnum);
}

termPo consHead(normalPo p) {
  assert(isCons((termPo) p));
  return nthArg(p, 0);
}

termPo consTail(normalPo p) {
  assert(isCons((termPo) p));
  return nthArg(p, 1);
}

void updateConsHead(termPo cns, termPo h) {
  assert(isCons((termPo) cns));
  normalPo p = C_NORMAL(cns);
  setArg(p, 0, h);
}

void updateConsTail(termPo cns, termPo t) {
  assert(isCons((termPo) cns));
  normalPo p = C_NORMAL(cns);
  setArg(p, 1, t);
}

integer consLength(termPo t) {
  integer ln = 0;
  while (isNormalPo(t) && termLbl(C_NORMAL(t)) == consCons) {
    t = nthArg(C_NORMAL(t), 1);
    ln++;
  }
  assert(sameTerm(t, nilEnum));
  return ln;
}

retCode dispCons(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  char *sep = "";
  retCode ret = outChar(out, '[');
  if (depth > 1) {
    while (isCons(t) && ret == Ok) {
      normalPo pr = C_NORMAL(t);

      ret = outStr(out, sep);
      if (ret == Ok)
        ret = dispTerm(out, consHead(pr), precision, depth - 1, alt);
      sep = ", ";
      t = consTail(pr);
    }
  } else {
    return outMsg(out, " ... ]");
  }
  if (ret == Ok && isConsNil(t)) {
    return outMsg(out, "]");
  } else
    return ret;
}
