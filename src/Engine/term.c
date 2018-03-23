//
// Created by Francis McCabe on 1/15/18.
//

#include "codeP.h"
#include <assert.h>
#include "formioP.h"
#include "labels.h"

SpecialClass SpecialClss = {
  .clss = Null,
  .sizeFun = Null,
  .copyFun = Null,
  .scanFun = Null,
  .compFun = Null,
  .hashFun = Null,
  .dispFun = Null
};

clssPo specialClass = (clssPo) &SpecialClss;

logical isSpecialClass(clssPo p) {
  return (logical) (p->clss == specialClass);
}

normalPo C_TERM(termPo t) {
  assert(isNormalPo(t));
  return (normalPo) t;
}

logical isNormalPo(termPo t) {
  return hasClass((termPo) t->clss, labelClass);
}

labelPo termLbl(normalPo t) {
  return t->lbl;
}

integer termArity(normalPo term) {
  labelPo lbl = term->lbl;
  return labelArity(lbl);
}

termPo nthArg(normalPo term, int64 ix) {
  assert(ix >= 0 && ix < termArity(term));
  return term->args[ix];
}

void setArg(normalPo term, int64 ix, termPo arg) {
  assert(ix >= 0 && ix < termArity(term));
  term->args[ix] = arg;
}

static retCode showTerm(ioPo f, void *data, long depth, long precision, logical alt) {
  return dispTerm(f, (termPo) data, depth, alt);
}

void initTerm() {
  installMsgProc('T', showTerm);
}

retCode dispTerm(ioPo out, termPo t, long depth, logical alt) {
  clssPo clss = t->clss;
  if (isSpecialClass(clss)) {
    specialClassPo spec = (specialClassPo) clss;
    return spec->dispFun(out, t, depth, alt);
  } else if (isNormalPo(t)) {
    normalPo nml = C_TERM(t);
    labelPo lbl = nml->lbl;
    retCode ret = outStr(out, labelName(lbl));
    if (ret == Ok)
      ret = outChar(out, '(');
    if (depth > 0) {
      char *sep = "";
      integer ar = labelArity(lbl);
      for (integer ix = 0; ix < ar && ret == Ok; ix++) {
        ret = outStr(out, sep);
        sep = ", ";
        if (ret == Ok)
          ret = dispTerm(out, nthArg(nml, ix), depth - 1, alt);
      }
    } else
      ret = outStr(out, "...");
    if (ret == Ok)
      ret = outChar(out, ')');
    return ret;
  } else
    return outMsg(out, "<<? 0x%x ?>>", t);
}

comparison compareTerm(termPo t1, termPo t2) {
  clssPo c1 = classOf(t1);
  clssPo c2 = classOf(t2);

  if (isSpecialClass(c1)) {
    if (isSpecialClass(c2)) {
      if (c1 == c2)
        return ((specialClassPo) c1)->compFun((specialClassPo) c1, t1, t2);
      else
        return incomparible;
    } else
      return incomparible;
  } else if (isSpecialClass(c2))
    return incomparible;
  else if (c1 != c2)
    return incomparible;
  else {
    normalPo n1 = C_TERM(t1);
    normalPo n2 = C_TERM(t2);
    labelPo lbl = n1->lbl;
    for (integer ix = 0; ix < labelArity(lbl); ix++) {
      comparison c = compareTerm(nthArg(n1, ix), nthArg(n2, ix));
      if (c != same)
        return c;
    }
    return same;
  }
}

integer termHash(termPo t) {
  clssPo c = classOf(t);

  if (isSpecialClass(c))
    return ((specialClassPo) c)->hashFun((specialClassPo) c, t);
  else {
    normalPo n1 = C_TERM(t);
    labelPo lbl = n1->lbl;
    integer hash = termHash((termPo) lbl);
    for (integer ix = 0; ix < labelArity(lbl); ix++)
      hash = hash * 37 + termHash(nthArg(n1, ix));

    return hash;
  }
}

// Special hash function used in case instruction. Only looks at the label of the term

integer hashTermLbl(termPo t) {
  clssPo c = classOf(t);

  if (isSpecialClass(c))
    return ((specialClassPo) c)->hashFun((specialClassPo) c, t);
  else
    return hashTermLbl((termPo) (C_TERM(t)->lbl));
}

integer termSize(normalPo t) {
  return NormalCellCount(labelArity(t->lbl));
}

normalPo allocateTpl(heapPo H, integer arity) {
  labelPo lbl = tplLabel(arity);
  int root = gcAddRoot(H, (ptrPo) &lbl);
  normalPo tpl = allocateStruct(H, lbl);
  gcReleaseRoot(H, root);
  return tpl;
}

normalPo allocatePair(heapPo H, termPo lhs, termPo rhs) {
  int root = gcAddRoot(H, &lhs);
  gcAddRoot(H, &rhs);
  normalPo tpl = allocateTpl(H, 2);
  setArg(tpl, 0, lhs);
  setArg(tpl, 1, rhs);
  gcReleaseRoot(H, root);
  return tpl;
}
