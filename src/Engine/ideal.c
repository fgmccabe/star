//
// Created by Francis McCabe on 2/19/22.
//

#include "ideal.h"
#include <labels.h>
#include <assert.h>
#include "cons.h"

termPo hNilEnum;
labelPo hLeafLbl, hNodeLbl;

void initIdeal() {
  hNilEnum = declareEnum("star.ideal#ihNil", 1, globalHeap);
  hLeafLbl = declareLbl("star.ideal#ihLeaf", 2, 0);
  hNodeLbl = declareLbl("star.ideal#ihNode", 4, 2);
}

logical isIdealEmpty(termPo t) {
  return sameTerm(t, hNilEnum);
}

logical isIdealTree(termPo t) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return (logical) (c->lbl == hLeafLbl || c->lbl == hNodeLbl);
  } else
    return isIdealEmpty(t);
}

static retCode dispTree(ioPo out, termPo t, integer precision, integer depth, logical alt, char **sep) {
  if (isIdealEmpty(t))
    return Ok;
  else if (isIdealTree(t)) {
    normalPo node = C_NORMAL(t);

    if (termLbl(node) == hLeafLbl) {
      termPo leafs = nthArg(node, 1);
      assert(isCons(leafs) || isConsNil(leafs));
      retCode ret = Ok;

      while (ret == Ok && isCons(leafs)) {
        termPo el = consHead(C_NORMAL(leafs));

        if (isNormalPo(el) && termArity(C_NORMAL(el)) == 2) {
          outStr(out, *sep);
          *sep = ", ";
          ret = dispTerm(out, nthArg(C_NORMAL(el), 0), precision, depth, alt);
          if (ret == Ok)
            ret = outStr(out, "->");
          if (ret == Ok)
            ret = dispTerm(out, nthArg(C_NORMAL(el), 1), precision, depth, alt);
        }
        leafs = consTail(C_NORMAL(leafs));
      }
      return ret;
    } else if (termLbl(node) == hNodeLbl) {
      retCode ret = dispTree(out, nthArg(node, 0), precision, depth, alt, sep);
      if (ret == Ok)
        ret = dispTree(out, nthArg(node, 1), precision, depth, alt, sep);
      if (ret == Ok)
        ret = dispTree(out, nthArg(node, 2), precision, depth, alt, sep);
      if (ret == Ok)
        ret = dispTree(out, nthArg(node, 3), precision, depth, alt, sep);
      return ret;
    } else
      return Error;
  } else
    return Error;
}

retCode dispIdeal(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  char *sep = "";
  retCode ret = outStr(out, "[");
  if (ret == Ok)
    ret = dispTree(out, t, precision, depth, alt, &sep);
  if (ret == Ok)
    ret = outStr(out, "]");
  return ret;
}

