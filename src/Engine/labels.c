//
// Created by Francis McCabe on 2/26/18.
//

#include "labelsP.h"
#include "codeP.h"
#include <stdlib.h>    /* access malloc etc. */

static hashPo tables;
static poolPo labelPool;
static poolPo labelTablePool;

static integer labelHash(labelPo lbl);
static comparison labelCmp(labelPo lb1, labelPo lb2);
static retCode labelDel(labelPo lbl, labelPo l);

static long lblSize(specialClassPo cl, termPo o);
static termPo lblCopy(specialClassPo cl, termPo dst, termPo src);
static termPo lblScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical lblCmp(specialClassPo cl, termPo o1, termPo o2);
static integer lblHash(specialClassPo cl, termPo o);
static retCode lblDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static retCode showFields(ioPo out, fieldTblPo tbl);

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

static labelPo newLbl(lblTablePo table, const char *name, integer arity) {
  labelPo lbl = (labelPo) allocPool(labelPool);
  lbl->arity = arity;
  lbl->name = uniDuplicate(name);
  lbl->len = uniStrLen(name);
  lbl->mtd = Null;
  lbl->clss = labelClass;
  lbl->hash = arity * 37 + uniHash(name);
  lbl->fields = Null;
  lbl->table = table;
  return lbl;
}

labelPo declareLbl(const char *name, integer arity) {
  lblTablePo tbl = locateLblTbl(name);

  assert(tbl != Null);
  check(arity >= 0, "invalid label arity");

  if (arity < LABELTABLE_COUNT) {
    labelPo lbl = tbl->arities[arity];

    if (lbl == Null) {
      lbl = newLbl(tbl, name, arity);
      tbl->arities[arity] = lbl;
    }

    return lbl;
  } else {
    if (tbl->otherEntries == Null) {
      tbl->otherEntries = newHash(32, ixHash, ixCmp, (destFun) Null);
    }
    labelPo lbl = (labelPo) hashGet(tbl->otherEntries, (void *) arity);
    if (lbl == Null) {
      lbl = newLbl(tbl, name, arity);
      hashPut(tbl->otherEntries, (void *) arity, lbl);
    }
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

labelPo declareEnum(const char *name) {
  return declareLbl(name, 0);
}

void declareFields(labelPo lbl, fieldTblPo tbl) {
  lbl->fields = tbl;
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
    ProcessTable(markLabel, tbl->otherEntries, c);
  }
  return Ok;
}

void markLabels(gcSupportPo G) {
  ProcessTable(markLabelTable, tables, G);
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
      ret = outMsg(out, "…%S", &lbl->name[hashOff + 1], lblLen - hashOff - 1);
    else if (lblLen > prec) {
      integer half = prec / 2;
      integer hwp = backCodePoint(lbl->name, lblLen, half);
      ret = outMsg(out, "%S…%S", lbl->name, half, &lbl->name[hwp], lblLen - hwp);
    } else
      ret = outMsg(out, "%S/%d", lbl->name, lblLen, lbl->arity);

    if (ret == Ok)
      ret = showFields(out, lbl->fields);

    return ret;
  } else if (lbl->arity > 0)
    return outMsg(out, "%s/%d", lbl->name, lbl->arity);
  else
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

logical isLabel(labelPo lbl, char *nm, integer arity) {
  return uniCmp(lbl->name, nm) == same && lbl->arity == arity;
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

logical isALabel(termPo t) {
  return hasClass(t, labelClass);
}

logical isRecordLabel(labelPo lbl) {
  return lbl->fields != Null;
}

integer fieldIndex(labelPo lbl, labelPo field) {
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

void setFieldTblEntry(fieldTblPo tbl, labelPo field, integer offset) {
  assert(offset >= 0 && offset < tbl->size);
  tbl->entries[offset].lbl = field;
  tbl->entries[offset].offset = offset;
}

void destroyFieldTable(fieldTblPo tbl) {
  free(tbl);
}

void clearFieldTable(labelPo lbl) {
  fieldTblPo fields = lbl->fields;
  lbl->fields = Null;
  destroyFieldTable(fields);
}

retCode applyFieldProc(labelPo lbl, integer ix, fieldProc p, void *cl) {
  assert(ix >= 0 && ix < lbl->fields->size);
  fieldPo fld = &lbl->fields->entries[ix];
  return p(fld->lbl, fld->offset, cl);
}

retCode showFields(ioPo out, fieldTblPo tbl) {
  retCode ret = Ok;

  if (tbl != Null) {
    char *sep = "";

    ret = outMsg(out, "{ ");
    for (integer ix = 0; ret == Ok && ix < tbl->size; ix++) {
      fieldPo fld = &tbl->entries[ix];
      ret = outMsg(out, "%s%d: %A", sep, fld->offset, fld->lbl);
      sep = "; ";
    }
    if (ret == Ok)
      ret = outMsg(out, " }");
  }
  return ret;
}
