//
// Created by Francis McCabe on 1/12/24.
//
#include "vectP.h"
#include "arith.h"
#include "assert.h"
#include "labelsP.h"
#include "normalP.h"

termPo eVect;
labelPo lf1, lf2, lf3, lf4;
labelPo vct1, vct2, vct3, vct4, vect;

void initVect() {
  eVect = declareEnum("star.vector#e", 0, globalHeap);
  lf1 = declareLbl("star.vector#lf1", 1, 1);
  lf2 = declareLbl("star.vector#lf2", 2, 2);
  lf3 = declareLbl("star.vector#lf3", 3, 3);
  lf4 = declareLbl("star.vector#lf4", 4, 4);
  vct1 = declareLbl("star.vector#vct1", 1, 5);
  vct2 = declareLbl("star.vector#vct2", 2, 6);
  vct3 = declareLbl("star.vector#vct3", 3, 7);
  vct4 = declareLbl("star.vector#vct4", 4, 8);
  vect = declareLbl("star.vector#vect", 2, 0);
}

void scanVect(gcSupportPo G) {
  eVect = markPtr(G, &eVect);
}

static logical isEVect(termPo t) {
  return sameTerm(t, eVect);
}

static logical isVLbl(termPo t, labelPo lbl) {
  if (isNormalPo(t)) {
    normalPo c = C_NORMAL(t);
    return (logical) (c->lbl == lbl);
  } else
    return False;
}

static logical isLf1(termPo t) {
  return isVLbl(t, lf1);
}

static logical isLf2(termPo t) {
  return isVLbl(t, lf2);
}

static logical isLf3(termPo t) {
  return isVLbl(t, lf3);
}

static logical isLf4(termPo t) {
  return isVLbl(t, lf4);
}

static logical isVct1(termPo t) {
  return isVLbl(t, vct1);
}

static logical isVct2(termPo t) {
  return isVLbl(t, vct2);
}

static logical isVct3(termPo t) {
  return isVLbl(t, vct3);
}

static logical isVct4(termPo t) {
  return isVLbl(t, vct4);
}

logical isVector(termPo t) {
  return isVLbl(t, vect);
}

static retCode dispVct(ioPo out, termPo t, integer precision, integer depth, logical alt, char **sep) {
  if (isEVect(t))
    return Ok;
  else if (depth <= 1)
    return outMsg(out, " ... ");
  else if (isLf1(t)) {
    retCode ret = outStr(out, *sep);
    if (ret == Ok)
      ret = dispTerm(out, nthElem(C_NORMAL(t), 0), precision, depth - 1, alt);
    return ret;
  } else if (isLf2(t)) {
    normalPo v = C_NORMAL(t);
    retCode ret = outStr(out, *sep);
    if (ret == Ok)
      ret = dispTerm(out, nthElem(v, 0), precision, depth - 1, alt);
    *sep = ", ";
    if (ret == Ok) {
      ret = outStr(out, *sep);
      if (ret == Ok)
        ret = dispTerm(out, nthElem(v, 1), precision, depth - 1, alt);
    }
    return ret;
  } else if (isLf3(t)) {
    normalPo v = C_NORMAL(t);
    retCode ret = outStr(out, *sep);
    if (ret == Ok)
      ret = dispTerm(out, nthElem(v, 0), precision, depth - 1, alt);
    *sep = ", ";
    if (ret == Ok) {
      ret = outStr(out, *sep);
      if (ret == Ok)
        ret = dispTerm(out, nthElem(v, 1), precision, depth - 1, alt);
    }
    if (ret == Ok) {
      ret = outStr(out, *sep);
      if (ret == Ok)
        ret = dispTerm(out, nthElem(v, 2), precision, depth - 1, alt);
    }
    return ret;
  } else if (isLf4(t)) {
    normalPo v = C_NORMAL(t);
    retCode ret = outStr(out, *sep);
    if (ret == Ok)
      ret = dispTerm(out, nthElem(v, 0), precision, depth - 1, alt);
    *sep = ", ";
    if (ret == Ok) {
      ret = outStr(out, *sep);
      if (ret == Ok)
        ret = dispTerm(out, nthElem(v, 1), precision, depth - 1, alt);
    }
    if (ret == Ok) {
      ret = outStr(out, *sep);
      if (ret == Ok)
        ret = dispTerm(out, nthElem(v, 2), precision, depth - 1, alt);
    }
    if (ret == Ok) {
      ret = outStr(out, *sep);
      if (ret == Ok)
        ret = dispTerm(out, nthElem(v, 3), precision, depth - 1, alt);
    }
    return ret;
  } else if (isVct1(t)) {
    normalPo v = C_NORMAL(t);
    return dispVct(out, nthElem(v, 0), precision, depth - 1, alt, sep);
  } else if (isVct2(t)) {
    normalPo v = C_NORMAL(t);
    retCode ret = dispVct(out, nthElem(v, 0), precision, depth - 1, alt, sep);
    if (ret == Ok)
      ret = dispVct(out, nthElem(v, 1), precision, depth - 1, alt, sep);
    return ret;
  } else if (isVct3(t)) {
    normalPo v = C_NORMAL(t);
    retCode ret = dispVct(out, nthElem(v, 0), precision, depth - 1, alt, sep);
    if (ret == Ok)
      ret = dispVct(out, nthElem(v, 1), precision, depth - 1, alt, sep);
    if (ret == Ok)
      ret = dispVct(out, nthElem(v, 2), precision, depth - 1, alt, sep);
    return ret;
  } else if (isVct4(t)) {
    normalPo v = C_NORMAL(t);
    retCode ret = dispVct(out, nthElem(v, 0), precision, depth - 1, alt, sep);
    if (ret == Ok)
      ret = dispVct(out, nthElem(v, 1), precision, depth - 1, alt, sep);
    if (ret == Ok)
      ret = dispVct(out, nthElem(v, 2), precision, depth - 1, alt, sep);
    if (ret == Ok)
      ret = dispVct(out, nthElem(v, 3), precision, depth - 1, alt, sep);
    return ret;
  } else {
    return outStr(out, "illegal vector element");
  }
}

retCode dispVect(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  if (depth > 1 && isVector(t)) {
    char *sep = "";
    retCode ret = outChar(out, '[');
    if (ret == Ok)
      ret = dispVct(out, nthElem(C_NORMAL(t), 1), precision, depth, alt, &sep);
    if (ret == Ok)
      ret = outStr(out, "]");
    return ret;
  } else
    return outMsg(out, "!%,*T!", displayDepth);
}

static void splitKey(integer key, integer dp, integer *fst, integer *rest) {
  integer s2 = dp - 2;
  *fst = (key >> s2) & 3;
  *rest = key & ((1 << s2) - 1);
}

static termPo pick(normalPo v, integer ix, integer key, integer dp);

static termPo lookup(normalPo v, integer key, integer dp) {
  if (dp > 0) {
    integer fst, rest;
    splitKey(key, dp, &fst, &rest);
    return pick(v, fst, rest, dp - 2);
  } else
    return Null;
}

static termPo pick(normalPo v, integer ix, integer key, integer dp) {
  termPo t = (termPo) v;
  switch (ix) {
    case 0:
      if (isVct1(t) || isVct2(t) || isVct3(t) || isVct4(t))
        return lookup(C_NORMAL(nthElem(v, 0)), key, dp);
      else if (isLf1(t) || isLf2(t) || isLf3(t) || isLf4(t))
        return nthElem(v, 0);
      else
        return Null;
    case 1:
      if (isVct2(t) || isVct3(t) || isVct4(t))
        return lookup(C_NORMAL(nthElem(v, 1)), key, dp);
      else if (isLf2(t) || isLf3(t) || isLf4(t))
        return nthElem(v, 1);
      else
        return Null;
    case 2:
      if (isVct3(t) || isVct4(t))
        return lookup(C_NORMAL(nthElem(v, 2)), key, dp);
      else if (isLf3(t) || isLf4(t))
        return nthElem(v, 2);
      else
        return Null;
    case 3:
      if (isVct4(t))
        return lookup(C_NORMAL(nthElem(v, 3)), key, dp);
      else if (isLf4(t))
        return nthElem(v, 3);
      else
        return Null;
    default:
      return Null;
  }
}

termPo vectElement(normalPo v, integer ix) {
  if (isVector((termPo) v)) {
    integer dp = integerVal(nthElem(v, 0));
    return lookup(C_NORMAL(nthElem(v, 1)), ix, dp);
  } else
    return Null;
}

static termPo allocateV1(heapPo H, labelPo lbl, termPo x0) {
  int root = gcAddRoot(H, &x0);
  normalPo v = allocateStruct(H, lbl);
  setArg(v, 0, x0);
  gcReleaseRoot(H, root);
  return (termPo) v;
}

static termPo allocateV2(heapPo H, labelPo lbl, termPo x0, termPo x1) {
  int root = gcAddRoot(H, &x0);
  gcAddRoot(H, &x1);
  normalPo v = allocateStruct(H, lbl);
  setArg(v, 0, x0);
  setArg(v, 1, x1);
  gcReleaseRoot(H, root);
  return (termPo) v;
}

static termPo allocateV3(heapPo H, labelPo lbl, termPo x0, termPo x1, termPo x2) {
  int root = gcAddRoot(H, &x0);
  gcAddRoot(H, &x1);
  gcAddRoot(H, &x2);
  normalPo v = allocateStruct(H, lbl);
  setArg(v, 0, x0);
  setArg(v, 1, x1);
  setArg(v, 2, x2);
  gcReleaseRoot(H, root);
  return (termPo) v;
}

static termPo allocateV4(heapPo H, labelPo lbl, termPo x0, termPo x1, termPo x2, termPo x3) {
  int root = gcAddRoot(H, &x0);
  gcAddRoot(H, &x1);
  gcAddRoot(H, &x2);
  gcAddRoot(H, &x3);
  normalPo v = allocateStruct(H, lbl);
  setArg(v, 0, x0);
  setArg(v, 1, x1);
  setArg(v, 2, x2);
  setArg(v, 3, x3);
  gcReleaseRoot(H, root);
  return (termPo) v;
}

static termPo grab(heapPo h, integer *ix, integer ln, integer dp, makeCB cb, void *cl);
static termPo grab1(heapPo h, termPo x0, termPo x1, termPo x2, integer *ix, integer ln, makeCB cb, void *cl);
static termPo grab2(heapPo h, termPo x0, termPo x1, integer *ix, integer ln, makeCB cb, void *cl);
static termPo grab3(heapPo h, termPo x0, integer *ix, integer ln, makeCB cb, void *cl);
static termPo grab4(heapPo h, integer *ix, integer ln, makeCB cb, void *cl);

termPo grab1(heapPo h, termPo x0, termPo x1, termPo x2, integer *ix, integer ln, makeCB cb, void *cl) {
  if (*ix < ln) {
    termPo el = cb(h, (*ix)++, cl);
    return allocateV4(h, lf4, x0, x1, x2, el);
  } else {
    return allocateV3(h, lf3, x0, x1, x2);
  }
}

termPo grab2(heapPo h, termPo x0, termPo x1, integer *ix, integer ln, makeCB cb, void *cl) {
  if (*ix < ln) {

    termPo el = cb(h, (*ix)++, cl);
    return grab1(h, x0, x1, el, ix, ln, cb, cl);
  } else {
    return allocateV2(h, lf2, x0, x1);
  }
}

termPo grab3(heapPo h, termPo x0, integer *ix, integer ln, makeCB cb, void *cl) {
  if (*ix < ln) {
    termPo el = cb(h, (*ix)++, cl);
    return grab2(h, x0, el, ix, ln, cb, cl);
  } else {
    return allocateV1(h, lf1, x0);
  }
}

termPo grab4(heapPo h, integer *ix, integer ln, makeCB cb, void *cl) {
  if (*ix < ln) {
    termPo el = cb(h, (*ix)++, cl);
    return grab3(h, el, ix, ln, cb, cl);
  } else {
    return eVect;
  }
}

termPo grab(heapPo h, integer *ix, integer ln, integer dp, makeCB cb, void *cl) {
  if (*ix < ln) {
    if (dp == 2)
      return grab4(h, ix, ln, cb, cl);
    else {
      assert(dp > 2);
      termPo t0 = grab(h, ix, ln, dp - 2, cb, cl);
      if (*ix >= ln)
        return allocateV1(h, vct1, (termPo) t0);
      else {
        int root = gcAddRoot(h, &t0);
        termPo t1 = grab(h, ix, ln, dp - 2, cb, cl);
        if (*ix >= ln) {
          gcReleaseRoot(h, root);
          return allocateV2(h, vct2, t0, t1);
        } else {
          gcAddRoot(h, &t1);
          termPo t2 = grab(h, ix, ln, dp - 2, cb, cl);
          if (*ix >= ln) {
            gcReleaseRoot(h, root);
            return allocateV3(h, vct3, t0, t1, t2);
          } else {
            gcAddRoot(h, &t2);
            termPo t3 = grab(h, ix, ln, dp - 2, cb, cl);
            gcReleaseRoot(h, root);
            return allocateV4(h, vct4, t0, t1, t2, t3);
          }
        }
      }
    }
  } else
    return eVect;
}

termPo makeVector(heapPo h, integer ln, makeCB cb, void *cl) {
  integer dp = ((lg2(ln * 2) + 1) >> 1) * 2;
  integer ix = 0;
  termPo v = grab(h, &ix, ln, dp, cb, cl);

  assert(ix == ln);
  return allocateV2(h, vect, makeInteger(dp), (termPo) v);
}
