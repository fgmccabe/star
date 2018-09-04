//
// Created by Francis McCabe on 2/26/18.
//

#include "labelsP.h"
#include "codeP.h"

static hashPo labels;
static poolPo labelPool;

static integer labelHash(labelPo lbl);
static comparison labelCmp(labelPo lb1, labelPo lb2);
static retCode labelDel(labelPo lbl, labelPo l);

static long lblSize(specialClassPo cl, termPo o);
static termPo lblCopy(specialClassPo cl, termPo dst, termPo src);
static termPo lblScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static comparison lblCmp(specialClassPo cl, termPo o1, termPo o2);
static integer lblHash(specialClassPo cl, termPo o);
static retCode lblDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass LabelClass = {
  .clss = Null,
  .sizeFun = lblSize,
  .copyFun = lblCopy,
  .scanFun = lblScan,
  .compFun = lblCmp,
  .hashFun = lblHash,
  .dispFun = lblDisp
};

clssPo labelClass = (clssPo) &LabelClass;

void initLbls() {
  LabelClass.clss = specialClass;
  labels = NewHash(1024, (hashFun) labelHash, (compFun) labelCmp, (destFun) labelDel);
  labelPool = newPool(sizeof(LblRecord), 1024);
}

labelPo declareLbl(const char *name, integer arity) {
  LblRecord tst = {.name=(char *) name, .arity=arity, .hash=hash64(arity * 37 + uniHash(name))};
  labelPo lbl = hashGet(labels, &tst);

  if (lbl == Null) {
    lbl = (labelPo) allocPool(labelPool);
    lbl->arity = arity;
    lbl->name = uniDuplicate(name);
    lbl->mtd = Null;
    lbl->clss = labelClass;
    lbl->hash = tst.hash;
    hashPut(labels, lbl, lbl);
  }
  return lbl;
}

labelPo declareEnum(const char *name) {
  return declareLbl(name, 0);
}

labelPo findLbl(const char *name, integer arity) {
  LblRecord tst = {.name=(char *) name, .arity=arity, .hash=hash64(arity * 37 + uniHash(name))};
  return hashGet(labels, &tst);
}

labelPo objLabel(labelPo lbl, integer arity) {
  return declareLbl(lbl->name, arity);
}

integer labelHash(labelPo lbl) {
  return lbl->hash;
}

comparison labelCmp(labelPo lb1, labelPo lb2) {
  comparison comp = uniCmp(lb1->name, lb2->name);

  if (comp == same) {
    if (lb1->arity < lb2->arity)
      comp = smaller;
    else if (lb1->arity > lb2->arity)
      comp = bigger;
  }
  return comp;
}

comparison lblCmp(specialClassPo cl, termPo o1, termPo o2) {
  labelPo i1 = C_LBL(o1);
  labelPo i2 = C_LBL(o2);

  return labelCmp(i1, i2);
}

static integer lblHash(specialClassPo cl, termPo o) {
  labelPo lbl = C_LBL(o);
  return labelHash(lbl);
}

retCode labelDel(labelPo lbl, labelPo l) {
  uniDestroy(lbl->name);
  freePool(labelPool, lbl);
  return Ok;
}

labelPo C_LBL(termPo t) {
  assert(hasClass(t, labelClass));

  return (labelPo) t;
}

static retCode markLabel(void *n, void *r, void *c) {
  labelPo lbl = (labelPo) r;
  gcSupportPo G = (gcSupportPo) c;

  // outMsg(logFile,"Marking label %T\n%_",lbl);

  if (lbl->mtd != Null)
    lbl->mtd = (methodPo) markPtr(G, (ptrPo) &lbl->mtd);
  return Ok;
}

void markLabels(gcSupportPo G) {
  ProcessTable(markLabel, labels, G);
}

long lblSize(specialClassPo cl, termPo o) {
  return LabelCellCount;
}

termPo lblCopy(specialClassPo cl, termPo dst, termPo src) {
  *((labelPo) dst) = *((labelPo) src);
  return dst + LabelCellCount;
}

termPo lblScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  labelPo lbl = C_LBL(o);

  if (lbl->mtd != Null)
    helper((ptrPo) (&lbl->mtd), c);

  return o + LabelCellCount;
}

retCode lblDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  labelPo lbl = C_LBL(t);
  return showLbl(out, depth, alt, lbl);
}

retCode showLbl(ioPo out, integer prec, logical alt, labelPo lbl) {
  integer lblLen = uniStrLen(lbl->name);
  if (alt) {
    if (lblLen > prec) {
      integer half = prec / 2;
      integer hwp = backCodePoint(lbl->name, lblLen, half);
      return outMsg(out, "%S…%S", lbl->name, half, &lbl->name[hwp], lblLen - hwp);
    } else
      return outMsg(out, "%S", lbl->name, lblLen);
  } else
    return outMsg(out, "%s/%d", lbl->name, lbl->arity);
}

methodPo labelCode(labelPo lbl) {
  return lbl->mtd;
}

integer labelArity(labelPo lbl) {
  return lbl->arity;
}

char *labelName(labelPo lbl) {
  return lbl->name;
}

labelPo tplLabel(integer arity) {
  char txt[MAX_SYMB_LEN];

  strMsg(txt, NumberOf(txt), "()%d", arity);
  return declareLbl(txt, arity);
}

logical isTplLabel(labelPo lb) {
  char *nm = lb->name;
  if (uniIsLitPrefix(nm, "()")) {
    integer ln = uniStrLen(nm);
    integer pos = uniStrLen("()");
    while (pos < ln) {
      codePoint cp = nextCodePoint(nm, &pos, ln);
      if (!isNdChar(cp))
        return False;
    }
    return True;
  } else
    return False;
}

logical isLabel(termPo t) {
  return hasClass(t, labelClass);
}
