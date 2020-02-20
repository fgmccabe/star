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
  assert(term != Null);
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

ptrPo termArgs(normalPo term) {
  return &term->args[0];
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

static retCode showField(labelPo fldLbl, integer offset, integer size, void *cl) {
  FieldInfoRec *info = (FieldInfoRec *) cl;
  tryRet(outMsg(info->out, "%T[%d] = ", fldLbl, offset));
  return dispTerm(info->out, nthArg(info->trm, offset), info->precision, info->depth, info->alt);
}

retCode dispTerm(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  clssPo clss = t->clss;
  if (isSpecialClass(clss)) {
    specialClassPo spec = (specialClassPo) clss;
    return spec->dispFun(out, t, precision, depth, alt);
  } else if (isNormalPo(t)) {
    normalPo nml = C_TERM(t);
    labelPo lbl = nml->lbl;
    if (isRecordLabel(lbl)) {
      FieldInfoRec Info = {.out = out, .depth=depth, .precision=precision, .alt=alt, .trm=nml};
      integer arity = labelArity(lbl);
      retCode ret = showLabel(out, lbl, depth, precision, alt);
      char *sep = "";
      if (ret == Ok)
        ret = outStr(out, "{ ");
      for (integer ix = 0; ret == Ok && ix < arity; ix++) {
        ret = outStr(out, sep);
        if (ret == Ok)
          ret = applyFieldProc(lbl, ix, showField, &Info);
        sep = "; ";
      }
      if (ret == Ok)
        ret = outStr(out, "}");
      return ret;
    } else {
      retCode ret = (isTplLabel(lbl) ? Ok : showLbl(out, lbl, 0, 24, alt));
      if (ret == Ok)
        ret = outChar(out, '(');
      if (depth > 0) {
        char *sep = "";
        integer ar = labelArity(lbl);
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
    normalPo n1 = C_TERM(t1);
    normalPo n2 = C_TERM(t2);
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

termPo getField(normalPo term, labelPo field) {
  integer offset = fieldIndex(term->lbl, field);
  if (offset >= 0)
    return nthArg(term, offset);
  else
    return Null;
}

retCode setField(normalPo term, labelPo field, termPo val) {
  integer offset = fieldIndex(term->lbl, field);
  if (offset >= 0) {
    setArg(term, offset, val);
    return Ok;
  } else
    return Error;
}
