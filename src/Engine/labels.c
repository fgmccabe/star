//
// Created by Francis McCabe on 2/26/18.
//

#include "labelsP.h"
#include "codeP.h"
#include <stdlib.h>    /* access malloc etc. */

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
    lbl->fields = Null;
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

labelPo findLbls(const char *name) {
  return Null;
}

void declareFields(labelPo lbl, fieldTblPo tbl) {
  lbl->fields = tbl;
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

  if (lbl->fields != Null) {
    fieldTblPo fields = lbl->fields;
    for (integer ix = 0; ix < fields->size; ix++) {
      fieldPo fld = &fields->entries[ix];
      if (fld->lbl != Null)
        markLabel(Null, fld->lbl, c);
    }
  }
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

  if (lbl->fields != Null) {
    fieldTblPo fields = lbl->fields;
    for (integer ix = 0; ix < fields->size; ix++) {
      fieldPo fld = &fields->entries[ix];
      if (fld->lbl != Null)
        scanTerm(c, (termPo) fld->lbl);
    }
  }

  return o + LabelCellCount;
}

retCode lblDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  labelPo lbl = C_LBL(t);
  return showLbl(out, depth, alt, lbl);
}

retCode showLbl(ioPo out, integer prec, logical alt, labelPo lbl) {
  integer lblLen = uniStrLen(lbl->name);
  if (alt) {
    integer hashOff = uniLastIndexOf(lbl->name, lblLen, (codePoint) '#');

    if (hashOff > 0 && hashOff < lblLen - 1)
      return outMsg(out, "…%S", &lbl->name[hashOff + 1], lblLen - hashOff - 1);
    else if (lblLen > prec) {
      integer half = prec / 2;
      integer hwp = backCodePoint(lbl->name, lblLen, half);
      return outMsg(out, "%S…%S", lbl->name, half, &lbl->name[hwp], lblLen - hwp);
    } else
      return outMsg(out, "%S/%d", lbl->name, lblLen, lbl->arity);
  } else
    return outMsg(out, "%s", lbl->name);
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

integer fieldOffset(labelPo lbl, labelPo field) {
  fieldTblPo fields = lbl->fields;
  if (fields == Null)
    return -1;
  else {
    integer mx = fields->size - 1;
    integer lx = 0;

    while (mx >= lx) {
      integer mid = (mx + lx) / 2;
      switch (labelCmp(field, fields->entries[mid].lbl)) {
        case same:
          return fields->entries[mid].offset;
        case smaller:
          mx = mid - 1;
          continue;
        case bigger:
          lx = mid + 1;
          continue;
        default:
          return -1;
      }
    }

    return -1;
  }
}

fieldTblPo newFieldTable(integer count) {
  fieldTblPo tbl = (fieldTblPo) malloc(sizeof(FieldTable) + count * sizeof(FieldBucket));
  tbl->size = count;
  return tbl;
}

void setFieldTblEntry(fieldTblPo tbl, integer ix, labelPo field, integer offset) {
  assert(ix >= 0 && ix < tbl->size);
  tbl->entries[ix].lbl = field;
  tbl->entries[ix].offset = offset;
}

void destroyFieldTable(fieldTblPo tbl) {
  free(tbl);
}

void clearFieldTable(labelPo lbl) {
  fieldTblPo fields = lbl->fields;
  lbl->fields = Null;
  destroyFieldTable(fields);
}
