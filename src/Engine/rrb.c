//
// Created by Francis McCabe on 6/1/18.
// Relaxed Radix Balanced Trees


#include "rrbP.h"
#include <assert.h>
#include <globals.h>
#include "heapP.h"

static long vectorSize(specialClassPo cl, termPo o);
static termPo vectorCopy(specialClassPo cl, termPo dst, termPo src);
static termPo vectorScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static comparison vectorCmp(specialClassPo cl, termPo o1, termPo o2);
static integer vectorHash(specialClassPo cl, termPo o);
static retCode vectorDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass VectorClass = {
  .clss = Null,
  .sizeFun = vectorSize,
  .copyFun = vectorCopy,
  .scanFun = vectorScan,
  .compFun = vectorCmp,
  .hashFun = vectorHash,
  .dispFun = vectorDisp
};

clssPo vectorClass = (clssPo) &VectorClass;

void initVectors() {
  VectorClass.clss = specialClass;
}

vectorPo C_VECT(termPo t) {
  assert(hasClass(t, vectorClass));
  return (vectorPo) t;
}

long vectorSize(specialClassPo cl, termPo o) {
  return VectorCellCount;
}

termPo vectorCopy(specialClassPo cl, termPo dst, termPo src) {
  vectorPo si = C_VECT(src);
  vectorPo di = (vectorPo) dst;
  *di = *si;

  return (termPo) di + VectorCellCount;
}

termPo vectorScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  vectorPo vector = C_VECT(o);

  for (int ix = 0; ix < VECT_ENTRIES; ix++) {
    helper(&vector->els[ix], c);
  }

  return o + VectorCellCount;
}

comparison vectorCmp(specialClassPo cl, termPo o1, termPo o2) {
  vectorPo l1 = C_VECT(o1);
  vectorPo l2 = C_VECT(o2);
  integer s1 = vectorCount(l1);
  integer s2 = vectorCount(l2);

  integer sz = minimum(s1, s2);

  for (integer ix = 0; ix < sz; ix++) {
    termPo e1 = nthEntry(l1, ix);
    termPo e2 = nthEntry(l2, ix);

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

static integer vectorDepth(vectorPo vect) {
  return vect->depth;
}

static integer localMax(vectorPo vect, int ix) {
  if (ix == 0)
    return vect->indices[0];
  else
    return (vect->indices[ix] - vect->indices[ix - 1]);
}

static int lastIndex(vectorPo vect) {
  for (int mx = VECT_ENTRIES - 1; mx >= 0; mx--) {
    if (vect->indices[mx] > -1)
      return mx;
  }
  return 0;
}

static int localIndex(vectorPo vect, integer index) {
  unsigned int lx = (unsigned int) ((((uinteger) index) >> ((uinteger) (vect->depth * VECT_SIZE))) & VECT_MASK);

  assert(lx >= 0 && lx < VECT_ENTRIES);
  while (vect->indices[lx] <= index && lx < VECT_ENTRIES)
    lx++;

  return (int) lx;
}

integer vectorCount(vectorPo vect) {
  return vect->indices[lastIndex(vect)];
}

static retCode procVec(vectorPo vect, integer *ex, int mx, vectorProc p, void *cl) {
  retCode ret = Ok;
  if (vectorDepth(vect) == 0) {
    for (int ix = 0; ret == Ok && ix < mx; ix++) {
      ret = p(vect->els[ix], (*ex)++, cl);
    }
  } else {
    for (int ix = 0; ret == Ok && ix < mx; ix++) {
      ret = procVec(C_VECT(vect->els[ix]), ex, lastIndex(vect), p, cl);
    }
  }
  return ret;
}

retCode processVector(vectorPo vect, vectorProc p, void *cl) {
  integer ix = 0;
  return procVec(vect, &ix, lastIndex(vect), p, cl);
}

retCode elHash(termPo el, integer ix, void *cl) {
  integer *h = (integer *) cl;
  *h = (*h) * 37 + termHash(el);
  return Ok;
}

integer vectorHash(specialClassPo cl, termPo o) {
  vectorPo v = C_VECT(o);

  integer hash = uniHash("array");

  processVector(v, elHash, (void *) &hash);

  return hash;
}

vectorPo allocVector(heapPo H, integer depth) {
  vectorPo v = (vectorPo) allocateObject(H, vectorClass, VectorCellCount);
  v->depth = depth;
  return v;
}

vectorPo copyVector(heapPo H, vectorPo v) {
  int root = gcAddRoot(H, (ptrPo) &v);
  vectorPo new = allocVector(H, v->depth);
  for (int ix = 0; ix < VECT_ENTRIES; ix++) {
    new->indices[ix] = v->indices[ix];
    new->els[ix] = v->els[ix];
  }
  return new;
}

termPo nthEntry(vectorPo vect, integer index) {
  assert(index >= 0 && index < vectorCount(vect));
  while (vect->depth > 0) {
    int ix = localIndex(vect, index);
    if (ix > 0)
      index -= vect->indices[ix - 1];
    vect = C_VECT(vect->els[ix]);
  }

  assert(index >= 0 && index < VECT_ENTRIES && vect->depth == 0);
  return vect->els[index];
}

static void unsafeSetNth(vectorPo vect, integer index, termPo el) {
  while (vect->depth > 0) {
    int ix = localIndex(vect, index);
    if (ix > 0) {
      index -= vect->indices[ix - 1];
    }
    vect = C_VECT(vect->els[ix]);
  }
  assert(index >= 0 && index < VECT_ENTRIES);
  vect->els[index] = el;
}

static vectorPo safeSetNth(heapPo H, vectorPo vect, integer index, termPo el) {
  int mark = gcAddRoot(H, (ptrPo) &vect);

  if (vect->depth > 0) {
    int ix = localIndex(vect, index);
    if (ix > 0) {
      index -= vect->indices[ix - 1];
    }
    vectorPo nEl = safeSetNth(H, C_VECT(vect->els[ix]), index, el);
    gcAddRoot(H, (ptrPo) &nEl);
    vectorPo nV = copyVector(H, vect);
    nV->els[ix] = (termPo) nEl;
    gcReleaseRoot(H, mark);
    return nV;
  } else {
    assert(index >= 0 && index < VECT_ENTRIES);

    gcAddRoot(H, (ptrPo) &el);
    vectorPo nV = allocVector(H, 0);
    for (int jx = 0; jx < VECT_ENTRIES; jx++) {
      nV->indices[jx] = vect->indices[jx];
      nV->els[jx] = vect->els[jx];
    }
    nV->els[index] = el;
    gcReleaseRoot(H, mark);
    return nV;
  }
}

vectorPo setNthEntry(heapPo H, vectorPo vect, integer index, termPo el, logical safeMode) {
  if (safeMode) {
    return safeSetNth(H, vect, index, el);
  } else {
    unsafeSetNth(vect, index, el);
    return vect;
  }
}

typedef struct {
  ioPo out;
  char *sep;
  integer precision;
  integer depth;
  logical alt;
} DispRec;

static retCode dispVecEl(termPo el, integer ix, void *cl) {
  DispRec *d = (DispRec *) cl;
  retCode ret = outStr(d->out, d->sep);
  if (ret == Ok) {
    d->sep = ",";
    ret = dispTerm(d->out, el, d->precision, d->depth, d->alt);
  }
  return ret;
}

retCode vectorDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  vectorPo vector = C_VECT(t);
  retCode ret = outChar(out, '[');

  if (ret == Ok) {
    if (depth > 0) {
      DispRec d = {.out = out, .sep = "", .precision=precision, .depth=depth - 1, .alt=alt};
      ret = processVector(vector, dispVecEl, &d);
    } else
      ret = outStr(out, "..");
  }

  if (ret == Ok)
    ret = outChar(out, ']');
  return ret;
}

void startVectFocus(vectorPo v, vFocusPo f) {
  indexedVectFocus(v, 0, f);
}

void endVectFocus(vectorPo v, vFocusPo f) {
  integer index = 0;
  for (int ix = 0; ix < VECT_ENTRIES; ix++) {
    if (v->indices[ix] > index)
      index = v->indices[ix];
  }
  indexedVectFocus(v, index, f);
}

void indexedVectFocus(vectorPo v, integer index, vFocusPo f) {
  f->tos = VECT_DEPTH;
  f->index = index;

  for (int dx = 0; dx < vectorDepth(v); dx++) {
    f->ixStack[--f->tos] = localIndex(v, index);
    index = index - localMax(v, f->ixStack[f->tos]);
  }
}

retCode stepFor(vFocusPo f, int dx, vectorPo vect) {
  if (vect->depth > 0) {
    const int pos = f->ixStack[dx];
    switch (stepFor(f, dx - 1, C_VECT(vect->els[pos]))) {
      case Ok:
        return Ok;
      default:
      case Eof:
        if (pos < lastIndex(vect)) {
          f->ixStack[dx]++;
          for (int ix = 1; ix <= vect->depth; ix++)
            f->ixStack[dx - ix] = 0;
          return Ok;
        } else
          return Eof;
    }
  } else {
    if (f->ixStack[dx] < lastIndex(vect)) {
      f->ixStack[dx]++;
      f->index++;
      return Ok;
    } else
      return Eof;
  }
}

retCode stepForward(vFocusPo f, vectorPo vect) {
  return stepFor(f, VECT_DEPTH - 1, vect);
}

retCode stepBk(vFocusPo f, int dx, vectorPo vect) {
  if (vect->depth > 0) {
    const int pos = f->ixStack[dx];
    const int depth = (int) (vect->depth);
    switch (stepBk(f, dx - 1, C_VECT(vect->els[pos]))) {
      case Ok:
        return Ok;
      default:
      case Eof:
        if (pos > 0) {
          f->ixStack[dx]--;
          vect = C_VECT(vect->els[f->ixStack[dx]]);
          for (int ix = 1; ix <= depth; ix++) {
            f->ixStack[dx - ix] = lastIndex(vect);
            vect = C_VECT(vect->els[f->ixStack[dx - ix]]);
          }
          return Ok;
        } else
          return Eof;
    }
  } else {
    if (f->ixStack[dx] > 0) {
      f->ixStack[dx]--;
      f->index--;
      return Ok;
    } else
      return Eof;
  }
}

retCode stepBack(vFocusPo f, vectorPo vect) {
  return stepBk(f, VECT_DEPTH - 1, vect);
}

termPo currentFocusElement(vFocusPo f, vectorPo vect) {
  int dx = VECT_DEPTH-1;
  while(vect->depth>0){
    int pos = f->ixStack[dx--];
    vect = C_VECT(vect->els[pos]);
  }
  return vect->els[f->ixStack[dx]];
}

vectorPo appendToVector(heapPo H, vectorPo vect, termPo el){
  if(vect->depth>0){

  }else if(lastIndex(vect)<VECT_ENTRIES){
    int mark = gcAddRoot(H,(ptrPo)&vect);
    gcAddRoot(H,&el);
    vectorPo vv = copyVector(H,vect);
    int last = lastIndex(vect);
    vv->els[last] = el;
    vv->indices[last] = last;
    gcReleaseRoot(H,mark);
    return vv;
  }

}
