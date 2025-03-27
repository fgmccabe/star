//
// Created by Francis McCabe on 7/19/20.
//

#include <ooio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include "arrayP.h"
#include "quick.h"

poolPo arrayPool = Null;

static void initArray() {
  if (arrayPool == Null) {
    arrayPool = newPool(sizeof(ArrayRecord), 256);
  }
}

static retCode resizeArray(arrayPo ar,integer request);
static void freeArrayData(arrayPo ar);

arrayPo allocArray(int elSize, integer initial, logical growable) {
  initArray();
  arrayPo ar = (arrayPo) allocPool(arrayPool);
  ar->elSize = elSize;
  ar->data = malloc(elSize * initial);
  ar->dataLength = elSize * initial;
  ar->count = 0;
  ar->grow = (growable?resizeArray:Null);
  ar->free = freeArrayData;
  return ar;
}

arrayPo fixedArray(int elSize, integer initial, void *data, arrayGrow grow, arrayRelease release) {
  initArray();
  arrayPo ar = (arrayPo) allocPool(arrayPool);
  ar->elSize = elSize;
  ar->data = data;
  ar->dataLength = elSize * initial;
  ar->count = 0;
  ar->grow = grow;
  ar->free = release;
  return ar;
}

static retCode ensureRoom(arrayPo ar, integer request) {
  assert(request >= 0);

  if (ar->dataLength < (ar->count+request) * ar->elSize) {
    if (ar->grow==Null || ar->grow(ar,request)!=Ok)
      return Error;
  }
  return Ok; // We have enough room
}

retCode resizeArray(arrayPo ar,integer request){
  integer newSize = ar->dataLength+request*ar->elSize;
  
  void *newData = realloc(ar->data, newSize);
  if (newData == Null)
    return Space;
  else {
    ar->data = newData;
    ar->dataLength = newSize;
    return Ok;
  }
}

void freeArrayData(arrayPo ar){
  free(ar->data);
}

retCode appendEntry(arrayPo ar, void *el) {
  if(ensureRoom(ar,1)==Ok){
    assert((ar->count + 1) * ar->elSize < ar->dataLength);
    void *tgt = ar->data + (ar->count * ar->elSize);
    memcpy(tgt, el, ar->elSize);
    ar->count++;
    return Ok;
  }
  else
    return Error;
}

void *newEntry(arrayPo ar) {
  if(ensureRoom(ar,1)==Ok){
    assert((ar->count + 1) * ar->elSize < ar->dataLength);
    ar->count++;
    return ar->data + ((ar->count-1) * ar->elSize);
  } else
    return Null;
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

arrayPo eraseArray(arrayPo ar, arrayElProc eraser, void *cl) {
  if (eraser != Null)
    processArrayElements(ar, eraser, cl);

  if (ar->free!=Null)
    ar->free(ar);
  freePool(arrayPool, ar);
  return Null;
}

retCode processArrayElements(arrayPo ar, arrayElProc proc, void *cl) {
  retCode ret = Ok;
  if (ar != Null) {
    for (integer ix = 0; ret == Ok && ix < ar->count; ix++) {
      void *el = ar->data + (ar->elSize * ix);
      ret = proc(el, ix, cl);
    }
  }
  return ret;
}

typedef struct {
  arrayPo src;
  compareEls compare;
  void *cl;
} ArrayCtx;

static comparison elComp(integer el1, integer el2, void *cl) {
  ArrayCtx *ctx = (ArrayCtx *) cl;
  return ctx->compare(ctx->src, el1, el2, ctx->cl);
}

static retCode swapEntry(integer el1, integer el2, void *cl) {
  ArrayCtx *ctx = (ArrayCtx *) cl;
  byte buffer[ctx->src->elSize];

  void *entry1 = nthEntry(ctx->src, el1);
  void *entry2 = nthEntry(ctx->src, el2);

  assert(entry1 != Null && entry2 != Null);
  memcpy(buffer, entry1, ctx->src->elSize);
  memcpy(entry1, entry2, ctx->src->elSize);
  memcpy(entry2, buffer, ctx->src->elSize);
  return Ok;
}

retCode sortArray(arrayPo ar, compareEls compare, void *cl) {
  ArrayCtx ctx = {.src=ar, .compare=compare, .cl=cl};
  return quick(0, arrayCount(ar)-1, elComp, swapEntry, (void *) &ctx);
}

retCode copyOutData(arrayPo ar, void *buffer, integer buffSize) {
  if (buffSize < ar->elSize * ar->count)
    return Space;
  else {
    memcpy(buffer, ar->data, buffSize);
    return Ok;
  }
}
