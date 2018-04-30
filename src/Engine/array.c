//
// Created by Francis McCabe on 3/4/18.
//

#include "arrayP.h"
#include <assert.h>
#include <globals.h>
#include "heapP.h"

static long sliceSize(specialClassPo cl, termPo o);
static termPo sliceCopy(specialClassPo cl, termPo dst, termPo src);
static termPo sliceScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static comparison sliceCmp(specialClassPo cl, termPo o1, termPo o2);
static integer sliceHash(specialClassPo cl, termPo o);
static retCode sliceDisp(ioPo out, termPo t, long depth, logical alt);

SpecialClass SliceClass = {
  .clss = Null,
  .sizeFun = sliceSize,
  .copyFun = sliceCopy,
  .scanFun = sliceScan,
  .compFun = sliceCmp,
  .hashFun = sliceHash,
  .dispFun = sliceDisp
};

clssPo listClass = (clssPo) &SliceClass;

static long baseSize(specialClassPo cl, termPo o);
static termPo baseCopy(specialClassPo cl, termPo dst, termPo src);
static termPo baseScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static comparison baseCmp(specialClassPo cl, termPo o1, termPo o2);
static integer baseHash(specialClassPo cl, termPo o);
static retCode baseDisp(ioPo out, termPo t, long depth, logical alt);

static termPo allocateBase(heapPo H, integer length, logical safeMode);

SpecialClass BaseClass = {
  .clss = Null,
  .sizeFun = baseSize,
  .copyFun = baseCopy,
  .scanFun = baseScan,
  .compFun = baseCmp,
  .hashFun = baseHash,
  .dispFun = baseDisp
};

clssPo baseClass = (clssPo) &BaseClass;

void initLists() {
  SliceClass.clss = specialClass;
  BaseClass.clss = specialClass;
}

listPo C_LIST(termPo t) {
  assert(hasClass(t, listClass));
  return (listPo) t;
}

long sliceSize(specialClassPo cl, termPo o) {
  return ListCellCount;
}

termPo sliceCopy(specialClassPo cl, termPo dst, termPo src) {
  listPo si = C_LIST(src);
  listPo di = (listPo) dst;
  *di = *si;

  return (termPo) di + ListCellCount;
}

termPo sliceScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  listPo list = C_LIST(o);

  helper(&list->base, c);

  return o + ListCellCount;
}

comparison sliceCmp(specialClassPo cl, termPo o1, termPo o2) {
  listPo l1 = C_LIST(o1);
  listPo l2 = C_LIST(o2);
  integer s1 = listSize(l1);
  integer s2 = listSize(l2);

  integer sz = minimum(s1, s2);

  for (integer ix = 0; ix < sz; ix++) {
    termPo e1 = nthEl(l1, ix);
    termPo e2 = nthEl(l2, ix);

    comparison cmp = compareTerm(e1, e2);
    if (cmp != same)
      return cmp;
  }
  if (s1 < s2)
    return smaller;
  else if (s1 > s2)
    return bigger;
  else
    return same;
}

integer sliceHash(specialClassPo cl, termPo o) {
  listPo l = C_LIST(o);
  integer sz = listSize(l);

  integer hash = uniHash("array");

  for (integer ix = 0; ix < sz; ix++)
    hash = hash * 37 + termHash(nthEl(l, ix));
  return hash;
}

retCode sliceDisp(ioPo out, termPo t, long depth, logical alt) {
  listPo list = C_LIST(t);
  basePo base = C_BASE(list->base);

  retCode ret = outChar(out, '[');

  if (depth > 0) {
    integer ix = list->start;
    integer lx = ix + list->length;

    char *sep = "";

    while (ret == Ok && ix < lx) {
      ret = outStr(out, sep);
      sep = ", ";
      if (ret == Ok)
        ret = dispTerm(out, base->els[ix], depth - 1, alt);
      ix++;
    }
  } else if (ret == Ok)
    ret = outStr(out, "..");

  if (ret == Ok)
    ret = outChar(out, ']');
  return ret;
}

integer listSize(listPo list) {
  return list->length;
}

termPo nthEl(listPo list, integer ix) {
  basePo base = C_BASE(list->base);

  assert(ix >= 0 && ix < list->length && ix + list->start <= (base->max - base->min));

  return base->els[list->start + ix];
}

void setNthEl(listPo list, integer ix, termPo el) {
  basePo base = C_BASE(list->base);

  assert(ix >= 0 && ix < list->length && ix + list->start <= (base->max - base->min));
  base->els[list->start + ix] = el;
}

retCode processList(listPo list, listProc p, logical safeMode, void *cl) {
  retCode ret = Ok;
  basePo base = C_BASE(list->base);

  for (integer ix = 0; ret == Ok && ix < list->length; ix++) {
    ret = p(base->els[list->start + ix], ix, cl);
  }
  return ret;
}

listPo allocateList(heapPo H, integer length, logical safeMode) {
  listPo list = (listPo) allocateObject(H, listClass, ListCellCount);
  int root = gcAddRoot(H, (ptrPo) &list);
  list->start = 0;
  list->length = length;
  list->base = voidEnum;

  integer extra = maxl(length >> 3, 1);
  basePo base = (basePo) allocateBase(H, length + extra, safeMode);

  base->min = extra / 2;
  base->max = base->min + length;
  list->base = (termPo) base;
  list->start = base->min;
  gcReleaseRoot(H, root);
  return list;
}

listPo createList(heapPo H, integer capacity) {
  listPo list = (listPo) allocateObject(H, listClass, ListCellCount);
  int root = gcAddRoot(H, (ptrPo) &list);
  integer extra = maxl(capacity >> 3, 1);
  list->start = extra / 2;
  list->length = 0;
  list->base = voidEnum;

  basePo base = (basePo) allocateBase(H, capacity + extra, False);

  base->min = base->max = list->start;

  list->base = (termPo) base;
  gcReleaseRoot(H, root);
  return list;
}

static termPo newSlice(heapPo H, basePo base, integer start, integer length) {
  int root = gcAddRoot(H, (ptrPo) &base);
  listPo slice = (listPo) allocateObject(H, listClass, ListCellCount);

  slice->base = (termPo) base;
  slice->start = start;
  slice->length = length;
  gcReleaseRoot(H, root);
  return (termPo) slice;
}

termPo sliceList(heapPo H, listPo list, integer from, integer count) {
  assert(from >= 0 && from + count <= list->length);
  int root = gcAddRoot(H, (ptrPo) &list);

  listPo slice = (listPo) newSlice(H, C_BASE(list->base), list->start + from, count);

  gcReleaseRoot(H, root);
  return (termPo) slice;
}

static termPo duplicateBase(heapPo H, basePo ob, integer delta);

listPo appendToList(heapPo H, listPo list, termPo el) {
  basePo base = C_BASE(list->base);
  int root = gcAddRoot(H, (ptrPo) (&base));
  gcAddRoot(H, (ptrPo) (&list));
  gcAddRoot(H, (ptrPo) (&el));

  if (base->max == list->start + list->length && base->max < base->length) {
    lockHeap(H);
    if (base->max == list->start + list->length && base->max < base->length) { // check after locking heap
      base->els[base->max++] = el;
      listPo slice = (listPo) newSlice(H, base, list->start, list->length + 1);
      gcReleaseRoot(H, root);
      releaseHeapLock(H);
      return slice;
    }
    releaseHeapLock(H);
  }
  basePo nb = (basePo) duplicateBase(H, base, (list->length>>3)+1);
  nb->els[nb->max++] = el;
  gcAddRoot(H, (ptrPo) (&nb));
  listPo slice = (listPo) newSlice(H, nb, nb->min, nb->max-nb->min);
  gcReleaseRoot(H, root);
  return slice;
}

// Extend the list 'in place' with a new element. Assumes caller knows
listPo addToList(heapPo H, listPo list, termPo el) {
  basePo base = C_BASE(list->base);
  int root = gcAddRoot(H, (ptrPo) (&base));
  gcAddRoot(H, (ptrPo) (&list));
  gcAddRoot(H, (ptrPo) (&el));

  if (base->max == list->start + list->length && base->max < base->length) {
    lockHeap(H);
    if (base->max == list->start + list->length && base->max < base->length) { // check after locking heap
      base->els[base->max++] = el;
      list->length++;
      gcReleaseRoot(H, root);
      releaseHeapLock(H);
      return list;
    }
    releaseHeapLock(H);
  }
  basePo nb = (basePo) duplicateBase(H, base, (list->length >> 3)+1);
  nb->els[nb->max++] = el;
  gcAddRoot(H, (ptrPo) (&nb));
  listPo slice = (listPo) newSlice(H, nb, nb->min, nb->max-nb->min);
  gcReleaseRoot(H, root);
  return slice;
}

termPo prependToList(heapPo H, listPo list, termPo el) {
  basePo base = C_BASE(list->base);
  int root = gcAddRoot(H, (ptrPo) (&base));
  gcAddRoot(H, (ptrPo) (&list));
  gcAddRoot(H, (ptrPo) (&el));

  if (base->min == list->start && base->min > 0) {
    lockHeap(H);
    if (base->max == list->start && base->min > 0) { // check after locking heap
      base->els[--base->min] = el;
      listPo slice = (listPo) newSlice(H, base, list->start - 1, list->length + 1);
      gcReleaseRoot(H, root);
      releaseHeapLock(H);
      return (termPo) slice;
    }
    releaseHeapLock(H);
  }
  basePo nb = (basePo) duplicateBase(H, base, (list->length>>3)+1);
  nb->els[--nb->min] = el;
  gcAddRoot(H, (ptrPo) (&nb));
  listPo slice = (listPo) newSlice(H, nb, nb->min, list->length+1);
  gcReleaseRoot(H, root);
  return (termPo) slice;
}

listPo concatList(heapPo H, listPo l1, listPo l2) {
  int root = gcAddRoot(H, (ptrPo) &l1);
  gcAddRoot(H, (ptrPo) (&l2));
  integer len1 = l1->length;
  integer len2 = l2->length;
  integer llen = len1 + len2;

  listPo reslt = createList(H, llen + (llen >> 1));

  for (integer ix = 0; ix < len1; ix++) {
    setNthEl(reslt, ix, nthEl(l1, ix));
  }
  for (integer ix = 0; ix < len2; ix++) {
    setNthEl(reslt, ix + len1, nthEl(l2, ix));
  }

  gcReleaseRoot(H, root);
  return reslt;
}

basePo C_BASE(termPo t) {
  assert(hasClass(t, baseClass));
  return (basePo) t;
}

long baseSize(specialClassPo cl, termPo o) {
  basePo base = C_BASE(o);
  return BaseCellCount(base->length);
}

termPo baseCopy(specialClassPo cl, termPo dst, termPo src) {
  basePo si = C_BASE(src);
  basePo di = (basePo) dst;
  *di = *si;

  integer len = si->length;
  for (integer ix = 0; ix < len; ix++) {
    di->els[ix] = si->els[ix];
  }

  return (termPo) di + BaseCellCount(len);
}

termPo baseScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  basePo base = C_BASE(o);
  integer ix = base->min;
  integer lx = base->max;

  retCode ret = Ok;
  while (ix < lx && ret == Ok) {
    ret = helper(&base->els[ix], c);
    ix++;
  }

  return o + BaseCellCount(base->length);
}

comparison baseCmp(specialClassPo cl, termPo o1, termPo o2) {
  if (o1 == o2)
    return same;
  else
    return incomparible;
}

integer baseHash(specialClassPo cl, termPo o) {
  return uniHash("array_base");
}

retCode baseDisp(ioPo out, termPo t, long depth, logical alt) {
  basePo base = C_BASE(t);

  retCode ret = outStr(out, "<");

  if (depth > 0) {
    integer ix = base->min;
    integer lx = base->max;

    char *sep = "";

    while (ret == Ok && ix < lx) {
      ret = outStr(out, sep);
      sep = ", ";
      if (ret == Ok)
        ret = dispTerm(out, base->els[ix], depth - 1, alt);
      ix++;
    }
  } else if (ret == Ok)
    ret = outStr(out, "..");

  if (ret == Ok)
    ret = outStr(out, ">");
  return ret;
}

termPo allocateBase(heapPo H, integer length, logical safeMode) {
  basePo base = (basePo) allocateObject(H, baseClass, BaseCellCount(length));

  base->min = length / 2;
  base->max = length / 2;
  base->length = length;

  if (safeMode) {
    for (integer ix = 0; ix < length; ix++)
      base->els[ix] = voidEnum;
  }
  return (termPo) base;
}

termPo duplicateBase(heapPo H, basePo ob, integer delta) {
  int root = gcAddRoot(H, (ptrPo) (&ob));
  integer newLen = ob->length+delta;
  basePo base = (basePo) allocateObject(H, baseClass, BaseCellCount(newLen));

  integer ocount = ob->max - ob->min;

  assert(ocount >= 0);

  integer extra = newLen - ocount;

  base->min = extra / 2;
  base->max = base->min + ocount;
  base->length = newLen;

  for (integer ix = 0; ix < ocount; ix++) {
    base->els[base->min + ix] = ob->els[ob->min + ix];
  }

  gcReleaseRoot(H, root);
  return (termPo) base;
}
