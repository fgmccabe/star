//
// Created by Francis McCabe on 1/15/18.
// Copyright (c) 2018 and beyond. Francis G. McCabe

#include "codeP.h"
#include <assert.h>
#include <labelsP.h>
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

normalPo C_NORMAL(termPo t) {
  assert(isNormalPo(t));
  return (normalPo) t;
}

logical isNormalPo(termPo t) {
  return hasClass((termPo) t->clss, labelClass);
}

logical hasLabel(normalPo n, char *name, integer arity) {
  return isLabel(n->lbl, name, arity);
}

labelPo termLbl(normalPo t) {
  return t->lbl;
}

integer termArity(normalPo term) {
  assert(term != Null);
  labelPo lbl = term->lbl;
  return labelArity(lbl);
}

termPo nthArg(normalPo term, int64 ix) {
  check(ix >= 0 && ix < termArity(term), "out of bounds");
  return term->args[ix];
}

termPo nthElem(normalPo term, integer ix) {
  return term->args[ix];
}

void setArg(normalPo term, int64 ix, termPo arg) {
  check(ix >= 0 && ix < termArity(term), "out of bounds");
  term->args[ix] = arg;
}

retCode showTerm(ioPo f, void *data, long depth, long precision, logical alt) {
  return dispTerm(f, (termPo) data, precision, depth, alt);
}

void initTerm() {
}

typedef struct {
  ioPo out;
  normalPo trm;
  integer precision;
  integer depth;
  logical alt;
} FieldInfoRec;

static retCode showField(labelPo fldLbl, integer offset, void *cl) {
  FieldInfoRec *info = (FieldInfoRec *) cl;
  tryRet(outMsg(info->out, "%T = ", fldLbl));
  return dispTerm(info->out, nthArg(info->trm, offset), info->precision, info->depth, info->alt);
}

static retCode showArgs(ioPo out, normalPo nml, integer precision, integer depth, logical alt) {
  retCode ret = outChar(out, '(');
  if (depth > 0) {
    char *sep = "";
    integer ar = termArity(nml);
    for (integer ix = 0; ix < ar && ret == Ok; ix++) {
      ret = outStr(out, sep);
      sep = ", ";
      if (ret == Ok)
        ret = dispTerm(out, nthArg(nml, ix), precision, depth - 1, alt);
    }
  } else
    ret = outStr(out, "...");
  if (ret == Ok)
    ret = outChar(out, ')');
  return ret;
}

retCode dispTerm(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  clssPo clss = t->clss;
  if (isSpecialClass(clss)) {
    specialClassPo spec = (specialClassPo) clss;
    return spec->dispFun(out, t, precision, depth, alt);
  } else if (isNormalPo(t)) {
    normalPo nml = C_NORMAL(t);
    labelPo lbl = nml->lbl;
    integer arity = labelArity(lbl);

    if (isTplLabel(lbl)) {
      return showArgs(out, nml, precision, depth, alt);
    } else if (arity == 0) {
      return outMsg(out, ".%Q", labelName(lbl));
    } else {
      retCode ret = showLbl(out, lbl, 0, 24, alt);
      if (ret == Ok)
        ret = showArgs(out, nml, precision, depth, alt);
      return ret;
    }
  } else
    return outMsg(out, "<<? 0x%x ?>>", t);
}

logical sameTerm(termPo t1, termPo t2) {
  clssPo c1 = classOf(t1);
  clssPo c2 = classOf(t2);

  if (c1 != c2)
    return False;
  else if (isSpecialClass(c1)) {
    return ((specialClassPo) c1)->compFun((specialClassPo) c1, t1, t2);
  } else {
    normalPo n1 = C_NORMAL(t1);
    normalPo n2 = C_NORMAL(t2);
    labelPo lbl = n1->lbl;
    for (integer ix = 0; ix < labelArity(lbl); ix++) {
      if (!sameTerm(nthArg(n1, ix), nthArg(n2, ix)))
        return False;
    }
    return True;
  }
}

integer termHash(termPo t) {
  clssPo c = classOf(t);

  if (isSpecialClass(c))
    return ((specialClassPo) c)->hashFun((specialClassPo) c, t);
  else {
    normalPo n1 = C_NORMAL(t);
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
    return hashTermLbl((termPo) (C_NORMAL(t)->lbl));
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

