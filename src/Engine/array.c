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
static retCode sliceDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

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
static retCode baseDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

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

retCode sliceDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  listPo list = C_LIST(t);
  basePo base = C_BASE(list->base);

  retCode ret = outChar(out, '[');

  if (depth > 0) {
    integer ix = list->start;
    integer lx = ix + list->length;
    integer maxIx = (precision == 0 ? list->length + 1 : precision);

    char *sep = "";

    while (ret == Ok && ix < lx && maxIx-- > 0) {
      ret = outStr(out, sep);
      sep = ", ";
      if (ret == Ok)
        ret = dispTerm(out, base->els[ix], precision, depth - 1, alt);
      ix++;
    }
    if (ret == Ok && maxIx <= 0) {
      ret = outStr(out, "..");
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

  assert(ix >= 0 && ix < list->length && ix + list->start <= base->max);

  return base->els[list->start + ix];
}

void setNthEl(listPo list, integer ix, termPo el) {
  basePo base = C_BASE(list->base);

  assert(ix >= 0 && ix < list->length && ix + list->start <= base->max);
  base->els[list->start + ix] = el;
}

retCode processList(listPo list, listProc p, void *cl) {
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

  integer extra = maximum(length / 8, 1);
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
  integer extra = maximum(capacity / 8, 1);
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

static basePo copyBase(heapPo H, basePo ob, integer from, integer count, integer delta);

static logical saneList(heapPo H, listPo l) {
  if (inHeap(H, (termPo) l)) {
    basePo b = C_BASE(l->base);
    if (inHeap(H, (termPo) b)) {
      if (inHeap(H, (termPo) &b->els[b->max - 1])) {
        if (l->start >= b->min) {
          if (l->start + l->length <= b->max) {
            if (b->max >= b->min) {
              return b->length >= 0;
            }
          }
        }
      }
    }
  }
  return False;
}

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

  basePo nb = copyBase(H, base, list->start, list->length, (list->length / 8) + 2);
  nb->els[nb->max++] = el;
  gcAddRoot(H, (ptrPo) (&nb));
  listPo slice = (listPo) newSlice(H, nb, nb->min, nb->max - nb->min);
  gcReleaseRoot(H, root);
  return slice;
}

// Extend the list 'in place' with a new element. Assumes caller knows
static listPo addToList(heapPo H, listPo list, termPo el) {
  basePo base = C_BASE(list->base);

  assert(base->max < base->length && base->length > list->start + list->length);

  base->els[base->max++] = el;
  list->length++;
  return list;
}

listPo prependToList(heapPo H, listPo list, termPo el) {
  basePo base = C_BASE(list->base);
  int root = gcAddRoot(H, (ptrPo) (&base));
  gcAddRoot(H, (ptrPo) (&list));
  gcAddRoot(H, (ptrPo) (&el));

//  logMsg(logFile, "list before prepend: %T", list);

  if (base->min == list->start && base->min > 0) {
    lockHeap(H);
    if (base->max == list->start && base->min > 0) { // check after locking heap
      base->els[--base->min] = el;
      listPo slice = (listPo) newSlice(H, base, list->start - 1, list->length + 1);
      gcReleaseRoot(H, root);
      releaseHeapLock(H);

      //logMsg(logFile, "slice after prepend: %T", slice);
      return slice;
    }
    releaseHeapLock(H);
  }
  basePo nb = copyBase(H, base, list->start, list->length, (list->length / 8) + 2);
  nb->els[--nb->min] = el;
  gcAddRoot(H, (ptrPo) (&nb));
  listPo slice = (listPo) newSlice(H, nb, nb->min, list->length + 1);

//  logMsg(logFile, "slice post prepend: %T", slice);
  gcReleaseRoot(H, root);
  return slice;
}

listPo insertListEl(heapPo H, listPo list, integer px, termPo vl) {
  basePo base = C_BASE(list->base);

  if (px <= 0)
    return prependToList(H, list, vl);
  else if (px >= listSize(list))
    return appendToList(H, list, vl);
  else {
    int root = gcAddRoot(H, (ptrPo) (&base));
    gcAddRoot(H, (ptrPo) (&list));
    gcAddRoot(H, (ptrPo) (&vl));

    integer delta = base->length / 8;
    integer newLen = base->length + delta + 1;
    basePo nb = (basePo) allocateObject(H, baseClass, BaseCellCount(newLen));
    integer ocount = list->length;

    assert(ocount >= 0);

    integer extra = newLen - ocount;

    nb->min = extra / 2 - 1;
    nb->max = nb->min + ocount + 1;
    nb->length = newLen;

    for (integer ix = 0; ix < px; ix++) {
      nb->els[nb->min + ix] = base->els[base->min + ix];
    }

    nb->els[nb->min + px] = vl;

    for (integer ix = px; ix < ocount; ix++) {
      nb->els[nb->min + ix + 1] = base->els[base->min + ix];
    }

    gcAddRoot(H, (ptrPo) (&nb));
    listPo slice = (listPo) newSlice(H, nb, nb->min, ocount + 1);
    gcReleaseRoot(H, root);
    releaseHeapLock(H);

    assert(saneList(H, slice));
    return slice;
  }
}

listPo replaceListEl(heapPo H, listPo list, integer px, termPo vl) {
  if (px >= listSize(list))
    return appendToList(H, list, vl);
  else if (px < 0)
    return prependToList(H, list, vl);
  else {
    basePo base = C_BASE(list->base);
    int root = gcAddRoot(H, (ptrPo) (&base));
    gcAddRoot(H, (ptrPo) (&list));
    gcAddRoot(H, (ptrPo) (&vl));
    integer delta = base->length / 8;

    basePo nb = copyBase(H, base, list->start, list->length, delta);

    nb->els[nb->min + px] = vl;

    gcAddRoot(H, (ptrPo) &nb);

    listPo slice = (listPo) newSlice(H, nb, nb->min, list->length);
    assert(saneList(H, slice));

    gcReleaseRoot(H, root);
    releaseHeapLock(H);
    return slice;
  }
}

listPo removeListEl(heapPo H, listPo list, integer px) {
  basePo base = C_BASE(list->base);
  int root = gcAddRoot(H, (ptrPo) (&base));
  gcAddRoot(H, (ptrPo) (&list));

  integer delta = base->length / 8;
  integer newLen = list->length + delta;
  basePo nb = (basePo) allocateObject(H, baseClass, BaseCellCount(newLen));
  integer ocount = list->length;

  assert(ocount >= 0);

  integer extra = newLen - ocount;

  nb->min = extra / 2;
  nb->max = nb->min + ocount-1;
  nb->length = newLen;

  for (integer ix = 0; ix < px; ix++) {
    nb->els[nb->min + ix] = base->els[list->start + ix];
  }

  for (integer ix = px + 1; ix < ocount; ix++) {
    nb->els[nb->min + ix] = base->els[list->start + ix];
  }

  gcAddRoot(H, (ptrPo) (&nb));
  listPo slice = (listPo) newSlice(H, nb, nb->min, list->length - 1);

  assert(saneList(H,slice));

  gcReleaseRoot(H, root);
  releaseHeapLock(H);
  return slice;
}

listPo concatList(heapPo H, listPo l1, listPo l2) {
  integer len1 = l1->length;
  integer len2 = l2->length;

  if (len1 == 0)
    return l2;
  else if (len2 == 0)
    return l1;
  else {
    int root = gcAddRoot(H, (ptrPo) &l1);
    gcAddRoot(H, (ptrPo) (&l2));

    integer llen = len1 + len2;

    listPo reslt = createList(H, llen + llen / 2);

    for (integer ix = 0; ix < len1; ix++) {
      reslt = addToList(H, reslt, nthEl(l1, ix));
    }
    for (integer ix = 0; ix < len2; ix++) {
      reslt = addToList(H, reslt, nthEl(l2, ix));
    }

    gcReleaseRoot(H, root);
    return reslt;
  }
}

static retCode countEls(termPo el, integer ix, void *cl) {
  integer *count = (integer *) cl;
  listPo ll = C_LIST(el);
  (*count) += listSize(ll);
  return Ok;
}

listPo flattenList(heapPo H, listPo l) {
  int root = gcAddRoot(H, (ptrPo) &l);
  integer llen = 0;

  processList(l, countEls, &llen);

  listPo reslt = createList(H, llen);

  for (integer ix = 0; ix < listSize(l); ix++) {
    listPo ll = C_LIST(nthEl(l, ix));
    for (integer jx = 0; jx < listSize(ll); jx++) {
      reslt = addToList(H, reslt, nthEl(ll, jx));
    }
  }

  assert(listSize(reslt) == llen);

  gcReleaseRoot(H, root);
  return reslt;
}

listPo reverseList(heapPo H, listPo l1) {
  int root = gcAddRoot(H, (ptrPo) &l1);
  integer len = l1->length;

  listPo reslt = createList(H, len);

  for (integer ix = 1; ix <= len; ix++) {
    reslt = addToList(H, reslt, nthEl(l1, len - ix));
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

retCode baseDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
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
        ret = dispTerm(out, base->els[ix], precision, depth - 1, alt);
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

basePo copyBase(heapPo H, basePo ob, integer from, integer count, integer delta) {
  int root = gcAddRoot(H, (ptrPo) (&ob));
  integer newLen = count + delta;
  basePo base = (basePo) allocateObject(H, baseClass, BaseCellCount(newLen));

  base->min = delta / 2;
  base->max = base->min + count;

  for (integer ix = 0; ix < count; ix++) {
    base->els[base->min + ix] = ob->els[from + ix];
  }

  base->length = newLen;

  gcReleaseRoot(H, root);
  return base;
}
