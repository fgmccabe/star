//
// Created by Francis McCabe on 2/26/18.
//

#include "labelsP.h"
#include "codeP.h"
#include <stdlib.h>    /* access malloc etc. */
#include <strings.h>

static hashPo tables;
static poolPo labelPool;
static poolPo labelTablePool;

static comparison labelCmp(labelPo lb1, labelPo lb2);
static retCode labelDel(labelPo lbl, labelPo l);

static long lblSize(specialClassPo cl, termPo o);
static termPo lblCopy(specialClassPo cl, termPo dst, termPo src);
static termPo lblScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical lblCmp(specialClassPo cl, termPo o1, termPo o2);
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

#define LABELTABLE_COUNT 8

typedef struct labelTable_ {
  char *lblName;
  labelPo arities[LABELTABLE_COUNT];
  hashPo otherEntries;
} LabelTableRecord;

void initLbls() {
  LabelClass.clss = specialClass;
  tables = newHash(1024, (hashFun) uniHash, (compFun) uniCmp, (destFun) Null);

  labelPool = newPool(sizeof(LblRecord), 1024);
  labelTablePool = newPool(sizeof(LabelTableRecord), 1024);
}

static lblTablePo locateLblTbl(const char *name) {
  lblTablePo tbl = (lblTablePo) hashGet(tables, (void *) name);

  if (tbl == Null) {
    tbl = (lblTablePo) allocPool(labelTablePool);
    tbl->lblName = uniDuplicate(name);
    for (integer ix = 0; ix < LABELTABLE_COUNT; ix++)
      tbl->arities[ix] = Null;
    tbl->otherEntries = Null;
    hashPut(tables, tbl->lblName, tbl);
  }
  return tbl;
}

static lblTablePo findLblTbl(const char *name) {
  return (lblTablePo) hashGet(tables, (void *) name);
}

static labelPo newLbl(lblTablePo table, const char *name, integer arity, integer index) {
  labelPo lbl = (labelPo) allocPool(labelPool);
  lbl->arity = arity;
  lbl->index = index;
  lbl->name = uniDuplicate(name);
  lbl->len = uniStrLen(name);
  lbl->mtd = Null;
  lbl->clss = labelClass;
  lbl->hash = arity * 37 + uniHash(name);
  lbl->table = table;
  return lbl;
}

labelPo declareLbl(const char *name, integer arity, integer index) {
  lblTablePo tbl = locateLblTbl(name);

  assert(tbl != Null);
  check(arity >= 0, "invalid label arity");

  if (arity < LABELTABLE_COUNT) {
    labelPo lbl = tbl->arities[arity];

    if (lbl == Null) {
      lbl = newLbl(tbl, name, arity, index);
      tbl->arities[arity] = lbl;
    }

    if (index != -1 && lbl->index == -1)
      lbl->index = index;
    return lbl;
  } else {
    if (tbl->otherEntries == Null) {
      tbl->otherEntries = newHash(32, ixHash, ixCmp, (destFun) Null);
    }
    labelPo lbl = (labelPo) hashGet(tbl->otherEntries, (void *) arity);
    if (lbl == Null) {
      lbl = newLbl(tbl, name, arity, index);
      hashPut(tbl->otherEntries, (void *) arity, lbl);
    }
    if (index != -1 && lbl->index == -1)
      lbl->index = index;
    return lbl;
  }
}

labelPo findLbl(const char *name, integer arity) {
  lblTablePo tbl = findLblTbl(name);

  if (tbl != Null) {
    if (arity >= 0 && arity < LABELTABLE_COUNT) {
      return tbl->arities[arity];
    } else if (tbl->otherEntries != Null)
      return (labelPo) hashGet(tbl->otherEntries, (void *) arity);
    else
      return Null;
  }
  return Null;
}

labelPo otherLbl(labelPo lbl, integer arity) {
  assert(lbl != Null && arity >= 0 && lbl->table != Null);
  lblTablePo tbl = lbl->table;

  if (arity < LABELTABLE_COUNT)
    return tbl->arities[arity];
  else if (tbl->otherEntries != Null)
    return (labelPo) hashGet(tbl->otherEntries, (void *) arity);
  else
    return Null;
}

termPo declareEnum(const char *name, integer index, heapPo H) {
  labelPo lbl = declareLbl(name, 0, index);
  int root = gcAddRoot(H, (ptrPo) &lbl);
  normalPo tpl = allocateStruct(H, lbl);
  gcReleaseRoot(H, root);
  return (termPo) tpl;
}

labelPo objLabel(labelPo lbl, integer arity) {
  return otherLbl(lbl, arity);
}

integer labelHash(labelPo lbl) {
  return lbl->hash;
}

comparison labelCmp(labelPo lb1, labelPo lb2) {
  comparison comp = unicodeCmp(lb1->name, lb1->len, lb2->name, lb2->len);

  if (comp == same) {
    if (lb1->arity < lb2->arity)
      comp = smaller;
    else if (lb1->arity > lb2->arity)
      comp = bigger;
  }
  return comp;
}

logical lblCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
}

static integer lblHash(specialClassPo cl, termPo o) {
  labelPo lbl = C_LBL(o);
  return labelHash(lbl);
}

logical sameLabel(labelPo l1, labelPo l2) {
  return (logical) (l1 == l2);
}

retCode labelDel(labelPo lbl, labelPo l) {
  uniDestroy(lbl->name);
  freePool(labelPool, lbl);
  return Ok;
}

labelPo C_LBL(termPo t) {
  assert(isLabelPo(t));

  return (labelPo) t;
}

logical isLabelPo(termPo t) {
  return hasClass(t, labelClass);
}

static retCode markLabel(void *n, void *r, void *c) {
  labelPo lbl = (labelPo) r;
  gcSupportPo G = (gcSupportPo) c;
  if (lbl->mtd != Null)
    lbl->mtd = (methodPo) markPtr(G, (ptrPo) &lbl->mtd);
  return Ok;
}

static retCode markLabelTable(void *n, void *r, void *c) {
  lblTablePo tbl = (lblTablePo) r;

  for (integer ix = 0; ix < LABELTABLE_COUNT; ix++) {
    labelPo lbl = tbl->arities[ix];
    if (lbl != Null)
      markLabel(Null, (void *) lbl, c);
  }
  if (tbl->otherEntries != Null) {
    processHashTable(markLabel, tbl->otherEntries, c);
  }
  return Ok;
}

void markLabels(gcSupportPo G) {
  processHashTable(markLabelTable, tables, G);
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
  return showLbl(out, lbl, 0, depth, alt);
}

retCode showLabel(ioPo f, void *data, long depth, long precision, logical alt) {
  return showLbl(f, C_LBL((termPo) data), depth, depth, alt);
}

retCode showLbl(ioPo out, labelPo lbl, integer depth, integer prec, logical alt) {
  integer lblLen = uniStrLen(lbl->name);
  if (alt) {
    retCode ret;

    integer hashOff = uniLastIndexOf(lbl->name, lblLen, (codePoint) '#');

    if (hashOff > 0 && hashOff < lblLen - 1)
      ret = outMsg(out, "…%S/%d", &lbl->name[hashOff + 1], lblLen - hashOff - 1, lbl->arity);
    else if (lblLen > prec) {
      integer half = prec / 2;
      integer hwp = backCodePoint(lbl->name, lblLen, half);
      ret = outMsg(out, "%S…%S/%d", lbl->name, half, &lbl->name[hwp], lblLen - hwp, lbl->arity);
    } else
      ret = outMsg(out, "%S/%d", lbl->name, lblLen, lbl->arity);

    return ret;
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

integer labelIndex(labelPo lbl) {
  return lbl->index;
}

logical isLabel(labelPo lbl, char *nm, integer arity) {
  if (lbl != Null)
    return uniCmp(lbl->name, nm) == same && lbl->arity == arity;
  else
    return False;
}

labelPo tplLabel(integer arity) {
  char txt[MAX_SYMB_LEN];

  strMsg(txt, NumberOf(txt), "()%d", arity);
  return declareLbl(txt, arity, 0);
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

logical isALabel(termPo t) {
  return hasClass(t, labelClass);
}
