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
static logical sliceCmp(specialClassPo cl, termPo o1, termPo o2);
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
static logical baseCmp(specialClassPo cl, termPo o1, termPo o2);
static integer baseHash(specialClassPo cl, termPo o);
static retCode baseDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

static termPo allocateBase(heapPo H, integer length, allocSafety mode);

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

arrayPo C_ARRAY(termPo t) {
  assert(hasClass(t, listClass));
  return (arrayPo) t;
}

long sliceSize(specialClassPo cl, termPo o) {
  return ListCellCount;
}

termPo sliceCopy(specialClassPo cl, termPo dst, termPo src) {
  arrayPo si = C_ARRAY(src);
  arrayPo di = (arrayPo) dst;
  *di = *si;

  return (termPo) di + ListCellCount;
}

termPo sliceScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  arrayPo list = C_ARRAY(o);

  helper(&list->base, c);

  return o + ListCellCount;
}

logical sliceCmp(specialClassPo cl, termPo o1, termPo o2) {
  arrayPo l1 = C_ARRAY(o1);
  arrayPo l2 = C_ARRAY(o2);
  integer s1 = arraySize(l1);
  integer s2 = arraySize(l2);

  integer sz = minimum(s1, s2);

  for (integer ix = 0; ix < sz; ix++) {
    termPo e1 = nthEl(l1, ix);
    termPo e2 = nthEl(l2, ix);

    if (!sameTerm(e1, e2))
      return False;
  }

  return (logical) (s1 == s2);
}

integer sliceHash(specialClassPo cl, termPo o) {
  arrayPo l = C_ARRAY(o);
  integer sz = arraySize(l);

  integer hash = uniHash("array");

  for (integer ix = 0; ix < sz; ix++)
    hash = hash * 37 + termHash(nthEl(l, ix));
  return hash;
}

retCode sliceDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  arrayPo list = C_ARRAY(t);
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

integer arraySize(arrayPo list) {
  return list->length;
}

termPo nthEl(arrayPo list, integer ix) {
  basePo base = C_BASE(list->base);

  assert(ix >= 0 && ix < list->length && ix + list->start <= base->max);

  return base->els[list->start + ix];
}

void setNthEl(arrayPo list, integer ix, termPo el) {
  basePo base = C_BASE(list->base);

  assert(ix >= 0 && ix < list->length && ix + list->start <= base->max);
  base->els[list->start + ix] = el;
}

retCode processArray(arrayPo list, arrayProc p, void *cl) {
  retCode ret = Ok;
  basePo base = C_BASE(list->base);

  for (integer ix = 0; ret == Ok && ix < list->length; ix++) {
    ret = p(base->els[list->start + ix], ix, cl);
  }
  return ret;
}

arrayPo allocateArray(heapPo H, integer length) {
  arrayPo list = (arrayPo) allocateObject(H, listClass, ListCellCount);
  int root = gcAddRoot(H, (ptrPo) &list);
  list->start = 0;
  list->length = length;
  list->base = voidEnum;

  integer extra = maximum(length / 8, 1);
  basePo base = (basePo) allocateBase(H, length + extra, safeAlloc);

  base->min = extra / 2;
  base->max = base->min + length;
  list->base = (termPo) base;
  list->start = base->min;
  gcReleaseRoot(H, root);
  return list;
}

arrayPo createArray(heapPo H, integer capacity) {
  arrayPo list = (arrayPo) allocateObject(H, listClass, ListCellCount);
  int root = gcAddRoot(H, (ptrPo) &list);
  integer extra = maximum(capacity / 8, 1);
  list->start = extra / 2;
  list->length = 0;
  list->base = voidEnum;

  basePo base = (basePo) allocateBase(H, capacity + extra, fastAlloc);

  base->min = base->max = list->start;

  list->base = (termPo) base;
  gcReleaseRoot(H, root);
  return list;
}

static termPo newSlice(heapPo H, basePo base, integer start, integer length) {
  int root = gcAddRoot(H, (ptrPo) &base);
  arrayPo slice = (arrayPo) allocateObject(H, listClass, ListCellCount);

  slice->base = (termPo) base;
  slice->start = start;
  slice->length = length;
  gcReleaseRoot(H, root);
  return (termPo) slice;
}

termPo sliceArray(heapPo H, arrayPo list, integer from, integer to) {
  assert(from >= 0 && to >= from && to - from <= list->length);
  int root = gcAddRoot(H, (ptrPo) &list);

  arrayPo slice = (arrayPo) newSlice(H, C_BASE(list->base), list->start + from, to - from);

  gcReleaseRoot(H, root);
  return (termPo) slice;
}

static basePo copyBase(heapPo H, basePo ob, integer from, integer count, integer delta);

static logical saneList(heapPo H, arrayPo l) {
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

// Extend the list 'in place' with a new element. Assumes caller knows
static arrayPo addToList(heapPo H, arrayPo list, termPo el) {
  basePo base = C_BASE(list->base);

  assert(base->max < base->length && base->length > list->start + list->length);

  base->els[base->max++] = el;
  list->length++;
  return list;
}

arrayPo appendToArray(heapPo H, arrayPo list, termPo el) {
  basePo base = C_BASE(list->base);
  int root = gcAddRoot(H, (ptrPo) (&base));
  gcAddRoot(H, (ptrPo) (&list));
  gcAddRoot(H, (ptrPo) (&el));

  if (base->max == list->start + list->length && base->max < base->length) {
    lockHeap(H);
    if (base->max == list->start + list->length && base->max < base->length) { // check after locking heap
      base->els[base->max++] = el;
      arrayPo slice = (arrayPo) newSlice(H, base, list->start, list->length + 1);
      gcReleaseRoot(H, root);
      releaseHeapLock(H);
      return slice;
    }
    releaseHeapLock(H);
  }

  basePo nb = copyBase(H, base, list->start, list->length, (list->length / 8) + 2);
  nb->els[nb->max++] = el;
  gcAddRoot(H, (ptrPo) (&nb));
  arrayPo slice = (arrayPo) newSlice(H, nb, nb->min, nb->max - nb->min);
  gcReleaseRoot(H, root);
  return slice;
}

arrayPo prependToArray(heapPo H, arrayPo list, termPo el) {
  basePo base = C_BASE(list->base);
  int root = gcAddRoot(H, (ptrPo) (&base));
  gcAddRoot(H, (ptrPo) (&list));
  gcAddRoot(H, (ptrPo) (&el));

//  logMsg(logFile, "list before prepend: %T", list);

  if (base->min == list->start && base->min > 0) {
    lockHeap(H);
    if (base->max == list->start && base->min > 0) { // check after locking heap
      base->els[--base->min] = el;
      arrayPo slice = (arrayPo) newSlice(H, base, list->start - 1, list->length + 1);
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
  arrayPo slice = (arrayPo) newSlice(H, nb, nb->min, list->length + 1);

//  logMsg(logFile, "slice post prepend: %T", slice);
  gcReleaseRoot(H, root);
  return slice;
}

arrayPo insertArrayEl(heapPo H, arrayPo list, integer px, termPo vl) {
  basePo base = C_BASE(list->base);

  if (px <= 0)
    return prependToArray(H, list, vl);
  else if (px >= arraySize(list))
    return appendToArray(H, list, vl);
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
    arrayPo slice = (arrayPo) newSlice(H, nb, nb->min, ocount + 1);
    gcReleaseRoot(H, root);
    releaseHeapLock(H);

    assert(saneList(H, slice));
    return slice;
  }
}

arrayPo replaceArrayEl(heapPo H, arrayPo list, integer px, termPo vl) {
  if (px >= arraySize(list))
    return appendToArray(H, list, vl);
  else if (px < 0)
    return prependToArray(H, list, vl);
  else {
    basePo base = C_BASE(list->base);
    int root = gcAddRoot(H, (ptrPo) (&base));
    gcAddRoot(H, (ptrPo) (&list));
    gcAddRoot(H, (ptrPo) (&vl));
    integer delta = base->length / 8;

    basePo nb = copyBase(H, base, list->start, list->length, delta);

    nb->els[nb->min + px] = vl;

    gcAddRoot(H, (ptrPo) &nb);

    arrayPo slice = (arrayPo) newSlice(H, nb, nb->min, list->length);
    assert(saneList(H, slice));

    gcReleaseRoot(H, root);
    releaseHeapLock(H);
    return slice;
  }
}

arrayPo spliceArray(heapPo H, arrayPo list, integer from, integer to, arrayPo rep) {
  assert(from >= 0 && to >= from && to <= list->length);
  int root = gcAddRoot(H, (ptrPo) &list);
  gcAddRoot(H, (ptrPo) &rep);

  integer llen = list->length - to + from + rep->length;

  arrayPo reslt = createArray(H, llen + llen / 2); // leave some headroom

  for (integer ix = 0; ix < from; ix++) {
    reslt = addToList(H, reslt, nthEl(list, ix));
  }
  for (integer ix = 0; ix < rep->length; ix++) {
    reslt = addToList(H, reslt, nthEl(rep, ix));
  }
  for (integer ix = to; ix < list->length; ix++) {
    reslt = addToList(H, reslt, nthEl(list, ix));
  }

  gcReleaseRoot(H, root);
  return reslt;
}

arrayPo removeArrayEl(heapPo H, arrayPo list, integer px) {
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
  nb->max = nb->min + ocount - 1;
  nb->length = newLen;

  for (integer ix = 0; ix < px; ix++) {
    nb->els[nb->min + ix] = base->els[list->start + ix];
  }

  for (integer ix = px + 1; ix < ocount; ix++) {
    nb->els[nb->min + ix] = base->els[list->start + ix];
  }

  gcAddRoot(H, (ptrPo) (&nb));
  arrayPo slice = (arrayPo) newSlice(H, nb, nb->min, list->length - 1);

  assert(saneList(H, slice));

  gcReleaseRoot(H, root);
  releaseHeapLock(H);
  return slice;
}

arrayPo concatArray(heapPo H, arrayPo l1, arrayPo l2) {
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

    arrayPo reslt = createArray(H, llen + llen / 2);

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
  arrayPo ll = C_ARRAY(el);
  (*count) += arraySize(ll);
  return Ok;
}

arrayPo flattenArray(heapPo H, arrayPo l) {
  int root = gcAddRoot(H, (ptrPo) &l);
  integer llen = 0;

  processArray(l, countEls, &llen);

  arrayPo reslt = createArray(H, llen);

  for (integer ix = 0; ix < arraySize(l); ix++) {
    arrayPo ll = C_ARRAY(nthEl(l, ix));
    for (integer jx = 0; jx < arraySize(ll); jx++) {
      reslt = addToList(H, reslt, nthEl(ll, jx));
    }
  }

  assert(arraySize(reslt) == llen);

  gcReleaseRoot(H, root);
  return reslt;
}

arrayPo reverseArray(heapPo H, arrayPo l1) {
  int root = gcAddRoot(H, (ptrPo) &l1);
  integer len = l1->length;

  arrayPo reslt = createArray(H, len);

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

logical baseCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);
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

termPo allocateBase(heapPo H, integer length, allocSafety mode) {
  basePo base = (basePo) allocateObject(H, baseClass, BaseCellCount(length));

  base->min = length / 2;
  base->max = length / 2;
  base->length = length;

  if (mode == safeAlloc) {
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
