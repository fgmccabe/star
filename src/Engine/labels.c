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
static retCode lblDisp(ioPo out, termPo t, long depth, logical alt);

SpecialClass LabelClass = {
  .clss = Null,
  .sizeFun = lblSize,
  .copyFun = lblCopy,
  .scanFun = lblScan,
  .dispFun = lblDisp
};

clssPo labelClass = (clssPo) &LabelClass;

void initLbls() {
  LabelClass.clss = specialClass;
  labels = NewHash(1024, (hashFun) labelHash, (compFun) labelCmp, (destFun) labelDel);
  labelPool = newPool(sizeof(LblRecord), 1024);
}

labelPo declareLbl(char *name, integer arity) {
  LblRecord tst = {.name=name, .arity=arity};
  labelPo lbl = hashGet(labels, &tst);

  if (lbl == Null) {
    lbl = (labelPo) allocPool(labelPool);
    lbl->arity = arity;
    lbl->name = uniDuplicate(name);
    lbl->mtd = Null;
    lbl->clss = labelClass;
    hashPut(labels, lbl, lbl);
  }
  return lbl;
}

labelPo declareEnum(char *name) {
  return declareLbl(name, 0);
}

labelPo findLbl(char *name, integer arity) {
  LblRecord tst = {.name=name, .arity=arity};
  return hashGet(labels, &tst);
}

integer labelHash(labelPo lbl) {
  return uniHash(lbl->name) * 37 + lbl->arity;
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
  heapPo h = (heapPo) c;

  if (lbl->mtd != Null)
    markMtd(h, lbl->mtd);
  return Ok;
}

void markLabels(heapPo heap) {
  ProcessTable(markLabel, labels, heap);
}

long lblSize(specialClassPo cl, termPo o) {
  return 0;
}

termPo lblCopy(specialClassPo cl, termPo dst, termPo src) {
  *dst = *src;
  return dst;
}

termPo lblScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return o+LabelCellCount;
}

retCode lblDisp(ioPo out, termPo t, long depth, logical alt) {
  labelPo lbl = C_LBL(t);
  return showLbl(out, lbl);
}

retCode showLbl(ioPo out, labelPo lbl) {
  return outMsg(out, "%s/%d", lbl->name, lbl->arity);
}
