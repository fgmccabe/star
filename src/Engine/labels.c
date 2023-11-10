//
// Created by Francis McCabe on 2/26/18.
//

#include "labelsP.h"
#include "codeP.h"
#include "match.h"
#include "quick.h"
#include <stdlib.h>

static hashPo labelHashTable;

integer maxLabels = 65536;

static long lblSize(specialClassPo cl, termPo o);
static termPo lblCopy(specialClassPo cl, termPo dst, termPo src);
static termPo lblScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical lblCmp(specialClassPo cl, termPo o1, termPo o2);
static integer lblHash(specialClassPo cl, termPo o);
static retCode lblDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static termPo lblFinalizer(specialClassPo class, termPo o);

SpecialClass LabelClass = {
  .clss = Null,
  .sizeFun = lblSize,
  .copyFun = lblCopy,
  .scanFun = lblScan,
  .finalizer = lblFinalizer,
  .compFun = lblCmp,
  .hashFun = lblHash,
  .dispFun = lblDisp
};

clssPo labelClass = (clssPo) &LabelClass;

static LblRecord *labelTable;
static integer lblTableTop = 0;

static integer LabelHash(labelRecordPo lbl) {
  return hash61(lbl->arity * 37 + uniHash(lbl->name));
}

static comparison LabelCmp(labelRecordPo l1, labelRecordPo l2) {
  comparison cmp = uniCmp(l1->name, l2->name);
  if (cmp == same) {
    if (l1->arity == l2->arity)
      return same;
    else
      return incomparible;
  }
  return cmp;
}

void initLbls() {
  LabelClass.clss = specialClass;
  initSpecial(labelLbl, &LabelClass);
  labelHashTable = newHash(1024, (hashFun) LabelHash, (compFun) LabelCmp, (destFun) Null);

  labelTable = (LblRecord *) malloc(sizeof(LblRecord) * maxLabels);
  lblTableTop = 0;
}

labelPo declareLbl(const char *name, integer arity, integer index) {
  LabelRecord Lbl = {.name=name, .arity=arity};
  labelPo lbl = hashGet(labelHashTable, &Lbl);

  if (lbl == Null) {
    if (lblTableTop >= maxLabels)
      syserr("label label exhausted");

    lbl = &labelTable[lblTableTop++];
    lbl->lbl.arity = (&Lbl)->arity;
    lbl->lbl.name = uniDuplicate((&Lbl)->name);
    lbl->len = uniStrLen((&Lbl)->name);
    lbl->index = index;
    lbl->mtd = Null;
    lbl->clss = labelClass;
    lbl->hash = LabelHash(&Lbl);
    lbl->breakPointSet = False;
    hashPut(labelHashTable, &lbl->lbl, lbl);
    return lbl;
  }

  if (index == -1 && isTplLabel(name))
    index = 0;

  check(arity >= 0, "invalid label arity");

  if (index != -1 && lbl->index == -1)
    lbl->index = index;

  return lbl;
}

labelPo findLbl(const char *name, integer arity) {
  LabelRecord lbl = {.name=name, .arity=arity};
  return (labelPo) hashGet(labelHashTable, &lbl);
}

termPo declareEnum(const char *name, integer index, heapPo H) {
  labelPo lbl = declareLbl(name, 0, index);
  int root = gcAddRoot(H, (ptrPo) &lbl);
  normalPo tpl = allocateStruct(H, lbl);
  gcReleaseRoot(H, root);
  return (termPo) tpl;
}

integer labelHash(labelPo lbl) {
  return lbl->hash;
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

labelPo C_LBL(termPo t) {
  assert(isLabelPo(t));

  return (labelPo) t;
}

logical isLabelPo(termPo t) {
  return hasClass(t, labelClass);
}

void markLabels(gcSupportPo G) {
  for (integer ix = 0; ix < lblTableTop; ix++) {
    labelPo lbl = &labelTable[ix];
    if (lbl->mtd != Null)
      lbl->mtd = (methodPo) markPtr(G, (ptrPo) &lbl->mtd);
  }
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

termPo lblFinalizer(specialClassPo class, termPo o) {
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
  const char *name = lbl->lbl.name;
  integer arity = lbl->lbl.arity;

  integer lblLen = uniStrLen(name);
  if (alt) {
    retCode ret;

    integer hashOff = uniLastIndexOf(name, lblLen, (codePoint) '#');

    if (hashOff > 0 && hashOff < lblLen - 1)
      ret = outMsg(out, "…%S", &name[hashOff + 1], (long) (lblLen - hashOff - 1), arity);
    else if (lblLen > prec && prec > 0) {
      integer half = prec / 2;
      integer hwp = backCodePoint(name, lblLen, half);
      ret = outMsg(out, "%S…%S", name, half, &name[hwp], lblLen - hwp, arity);
    } else
      ret = outMsg(out, "%S", name, lblLen, arity);

    return ret;
  } else
    return outMsg(out, "%Q/%d", name, arity);
}

void showAllLabels() {
  for (integer ix = 0; ix < lblTableTop; ix++) {
    outMsg(logFile, "%A\n", &labelTable[ix]);
  }
  flushOut();
}

methodPo labelCode(labelPo lbl) {
  return lbl->mtd;
}

logical labelDefined(labelPo lbl) {
  return lbl->mtd != Null;
}

integer labelArity(labelPo lbl) {
  return lbl->lbl.arity;
}

const char *labelName(labelPo lbl) {
  return lbl->lbl.name;
}

integer labelIndex(labelPo lbl) {
  return lbl->index;
}

logical isLabel(labelPo lbl, char *nm, integer arity) {
  if (lbl != Null)
    return uniCmp(lbl->lbl.name, nm) == same && lbl->lbl.arity == arity;
  else
    return False;
}

labelPo tplLabel(integer arity) {
  char txt[MAX_SYMB_LEN];

  strMsg(txt, NumberOf(txt), "()%d", arity);
  return declareLbl(txt, arity, 0);
}

logical isTplLabel(const char *nm) {
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

retCode iterateLabels(labelProc proc, void *cl) {
  for (integer ix = 0; ix < lblTableTop; ix++)
    tryRet(proc(&labelTable[ix], cl));
  return Ok;
}

logical breakPointSet(labelPo lbl) {
  return lbl->breakPointSet;
}

logical setBreakPoint(labelPo lbl, logical set) {
  logical previous = lbl->breakPointSet;
  lbl->breakPointSet = set;
  return previous;
}

typedef struct {
  char *search;
  integer len;
  integer arity;
  logical set;
  integer count;
} SearchInfo;

static retCode checkLabel(labelPo lbl, void *cl) {
  SearchInfo *info = (SearchInfo *) cl;
  if (globMatch((char *) lbl->lbl.name, lbl->len, info->search, info->len)) {
    if (info->arity < 0 || info->arity == lbl->lbl.arity) {
      if (setBreakPoint(lbl, info->set) != info->set)
        info->count++;
    }
  }
  return Ok;
}

integer setLabelBreakPoint(char *srch, integer slen, integer arity) {
  SearchInfo info = {.search = srch, .len=slen, .arity=arity, .set=True, .count=0};
  iterateLabels(checkLabel, &info);
  return info.count;
}

integer clearLabelBreakPoint(char *srch, integer slen, integer arity) {
  SearchInfo info = {.search = srch, .len=slen, .arity=arity, .set=False, .count=0};
  iterateLabels(checkLabel, &info);
  return info.count;
}

static retCode showBkPt(labelPo lbl, void *cl) {
  if (lbl->breakPointSet)
    return outMsg((ioPo) cl, "bk: %L\n%_", lbl);
  return Ok;
}

retCode showLabelBreakPoints(ioPo out) {
  return iterateLabels(showBkPt, out);
}

static retCode showMtdCount(labelPo lbl, void *cl) {
  ioPo out = (ioPo) cl;
  methodPo mtd = labelCode(lbl);
  if (mtd != Null && callCount(mtd) > 0) {
    return outMsg(out, "%L %ld\n", lbl, callCount(mtd));
  } else
    return Ok;
}

static comparison cmpCount(integer i, integer j, void *cl) {
  integer *indices = (integer *) cl;
  methodPo mi = labelCode(&labelTable[indices[i]]);
  methodPo mj = labelCode(&labelTable[indices[j]]);

  integer iCount = (mi == Null ? 0 : callCount(mi));
  integer jCount = (mj == Null ? 0 : callCount(mj));

  if (iCount < jCount)
    return smaller;
  else if (iCount == jCount)
    return same;
  else
    return bigger;
}

static retCode swapIndex(integer i, integer j, void *cl) {
  integer *indices = (integer *) cl;
  integer w = indices[i];
  indices[i] = indices[j];
  indices[j] = w;
  return Ok;
}

void showMtdCounts(ioPo out) {
  outMsg(out, "sorted method counts\n");

  integer indices[lblTableTop];
  for (int ix = 0; ix < lblTableTop; ix++)
    indices[ix] = ix;

  quick(0, lblTableTop-1, cmpCount, swapIndex, (void *) indices);
  for (integer ix = 0; ix < lblTableTop; ix++) {
    showMtdCount(&labelTable[indices[ix]], out);
  }
}
