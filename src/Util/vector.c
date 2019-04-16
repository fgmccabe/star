//
// Created by Francis McCabe on 7/31/18.
//
// Vector structures

#include "vectorP.h"
#include "utils.h"
#include <assert.h>
#include <stdlib.h>

static integer vectorHash(objectPo o);

static logical vectorEquality(objectPo o1, objectPo o2);

static void vectorInit(objectPo o, va_list *args);

static void vectorDestroy(objectPo o);

VectorClassRec VectorClass = {
  {
    (classPo) &ObjectClass,
    "vector",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    vectorDestroy,
    O_INHERIT_DEF,
    vectorInit,
    sizeof(VectorObjRecord),
    vectorHash,
    vectorEquality,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo vectorClass = (classPo) &VectorClass;

void vectorInit(objectPo o, va_list *args) {
  vectorPo l = O_VECT(o);

  l->vect.count = 0;
  l->vect.size = 0;
  l->vect.data = Null;
}

void vectorDestroy(objectPo o) {
  vectorPo v = O_VECT(o);

  for (integer ix = 0; ix < vectLength(v); ix++) {
    decReference(getVectEl(v, ix));
  }
}

static integer vectorHash(objectPo o) {
  vectorPo v = O_VECT(o);

  integer hash = 0;
  for (integer ix = 0; ix < vectLength(v); ix++) {
    hash = hash * 37 + hashCode(getVectEl(v, ix));
  }
  return hash;
}

static logical vectorEquality(objectPo o1, objectPo o2) {
  vectorPo v1 = O_VECT(o1);
  vectorPo v2 = O_VECT(o2);

  if (vectLength(v1) != vectLength(v2))
    return False;
  else {
    for (integer ix = 0; ix < vectLength(v1); ix++) {
      if (!equals(getVectEl(v1, ix), getVectEl(v2, ix)))
        return False;
    }
    return True;
  }
}

vectorPo vector(int count, ...) {
  va_list args;

  va_start(args, count);

  vectorPo v = O_VECT(makeObject(vectorClass, &args));

  for (int ix = 0; ix < count; ix++) {
    objectPo arg = va_arg(args, objectPo);
    appendVectEl(v, arg);
  }

  va_end(args);

  return v;
}

objectPo getVectEl(vectorPo v, integer ix) {
  assert(ix >= 0 && ix < v->vect.count && ix < v->vect.size);
  return v->vect.data[ix];
}

retCode addVectEl(vectorPo v, integer off, objectPo el) {
  assert(off >= 0 && el != Null);

  if (v->vect.count == v->vect.size) {
    integer nSize = ((v->vect.size + 1) * 3) / 2;
    objectPo *nvect = realloc(v->vect.data, nSize * sizeof(objectPo));
    if (nvect == Null)
      return Error;
    else {
      v->vect.data = nvect;
      v->vect.size = nSize;
    }
  }

  if (off >= v->vect.count) {
    v->vect.data[v->vect.count++] = el;
  } else {
    for (integer ix = v->vect.count; ix >= off; ix--) {
      v->vect.data[ix] = v->vect.data[ix - 1];
    }
    v->vect.data[off] = el;
    v->vect.count++;
  }
  return Ok;
}

objectPo replaceVectEl(vectorPo v, integer off, objectPo el) {
  assert(off >= 0 && off < v->vect.count && el != Null);

  objectPo old = v->vect.data[off];
  v->vect.data[off] = el;

  return old;
}

retCode appendVectEl(vectorPo v, objectPo el) {
  if (v->vect.count == v->vect.size) {
    integer nSize = ((v->vect.size + 1) * 3) / 2;
    v->vect.data = realloc(v->vect.data, nSize * sizeof(objectPo));
    v->vect.size = nSize;
  }
  v->vect.data[v->vect.count++] = el;
  return Ok;
}

retCode pushVectEl(vectorPo v, objectPo el) {
  return appendVectEl(v, el);
}

objectPo popVectEl(vectorPo v) {
  return removeVectEl(v, v->vect.count - 1);
}

objectPo removeVectEl(vectorPo v, integer off) {
  assert(v != Null && off >= 0 && off < v->vect.count);
  objectPo el = v->vect.data[off];

  for (integer ix = off + 1; ix < v->vect.count; ix++)
    v->vect.data[ix - 1] = v->vect.data[ix];
  v->vect.count--;
  return el;
}

integer vectLength(vectorPo v) {
  return v->vect.count;
}

logical vectIsEmpty(vectorPo v){
  return (logical)(v->vect.count==0);
}
