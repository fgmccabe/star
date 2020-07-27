//
// Created by Francis McCabe on 7/19/20.
//

#include <ooio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "config.h"
#include "arrayP.h"

poolPo arrayPool = Null;

static void initArray() {
  if (arrayPool == Null) {
    arrayPool = newPool(sizeof(ArrayRecord), 256);
  }
}

arrayPo allocArray(int elSize, integer initial, logical growable) {
  initArray();
  arrayPo ar = (arrayPo) allocPool(arrayPool);
  ar->elSize = elSize;
  ar->data = malloc(elSize * initial);
  ar->dataLength = elSize * initial;
  ar->count = 0;
  ar->growable = growable;
  return ar;
}

arrayPo fixedArray(int elSize, integer initial, void *data) {
  initArray();
  arrayPo ar = (arrayPo) allocPool(arrayPool);
  ar->elSize = elSize;
  ar->data = data;
  ar->dataLength = elSize * initial;
  ar->count = 0;
  ar->growable = False;
  return ar;
}

retCode appendEntry(arrayPo ar, void *el) {
  if (ar->elSize * (ar->count + 1) > ar->dataLength) {
    if (ar->growable) {
      integer newSize = ar->dataLength + ar->dataLength / 2 + ar->elSize;
      void *newData = realloc(ar->data, newSize);
      if (newData == Null)
        return Space;
      else {
        ar->data = newData;
        ar->dataLength = newSize;
      }
    } else
      return Error;
  }
  assert((ar->count + 1) * ar->elSize < ar->dataLength);
  void *tgt = ar->data + (ar->count * ar->elSize);
  memcpy(tgt, el, ar->elSize);
  ar->count++;
  return Ok;
}

integer arrayCount(arrayPo ar) {
  return ar->count;
}

void *nthEntry(arrayPo ar, integer ix) {
  assert(ix >= 0 && ix < ar->count);
  return ar->data + (ar->elSize * ix);
}

retCode dropEntry(arrayPo ar, integer ix) {
  assert(ix >= 0 && ix < ar->count);
  void *tgt = nthEntry(ar, ix);
  void *src = nthEntry(ar, ix + 1);
  memmove(tgt, src, ar->elSize * (ar->count - ix - 1));
  return Ok;
}

arrayPo eraseArray(arrayPo ar) {
  if (ar->growable)
    free(ar->data);
  freePool(arrayPool, ar);
  return Null;
}

retCode processArrayElements(arrayPo ar, arrayElProc proc, void *cl) {
  retCode ret = Ok;
  for (integer ix = 0; ret == Ok && ix < ar->count; ix++) {
    void *el = ar->data + (ar->elSize * ix);
    ret = proc(el, ix, cl);
  }
  return ret;
}
