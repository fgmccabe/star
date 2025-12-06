#include <assert.h>

#include "setP.h"
#include "pool.h"
#include "ooio.h"
#include "utils.h"
#include <stdlib.h>
#include <arm/param.h>

static poolPo setPool = Null;

static void initSets() {
  if (setPool == Null) {
    setPool = newPool(sizeof(SetRecord), 64);
  }
}

setPo createSet(int32 min) {
  initSets();

  setPo set = allocPool(setPool);
  set->count = 0;
  set->min = min;
  set->data = Null;

  return set;
}

void deleteSet(setPo s) {
  if (s->data != Null)
    free(s->data);
  freePool(setPool, s);
}

static int32 align64(int32 x) {
  return (x + 64) & (~63);
}

retCode addToSet(setPo set, int32 k) {
  int32 max = set->count * 64 + set->min;
  if (k >= set->min && k < max) {
    int32 base = k - set->min;
    int32 el = base >> 6;
    uinteger mask = 1ul << (((uinteger) base) & 63);
    set->data[el] |= mask;
    return Ok;
  }
  if (set->data == Null) {
    int32 mx = align64(k);
    set->count = 1;
    set->min = mx - 64;
    assert(k>=set->min && k<mx);
    set->data = malloc(sizeof(uinteger) * set->count);
    for (int32 ix = 0; ix < set->count; ix++)
      set->data[ix] = 0;
    return addToSet(set, k);
  }

  if (k < set->min) {
    int32 newBase = align64(k-64);
    int32 oldMax = set->count * 64 + set->min;
    int32 newCount = (oldMax - newBase) / 64;

    assert(k>=newBase && k<newBase+64);

    uinteger *newData = (uinteger *) malloc(sizeof(uinteger) * newCount);
    for (int32 ix = 0; ix < newCount-set->count; ix++) {
      newData[ix] = 0;
    }
    for (int32 ix = 0; ix < set->count; ix++) {
      newData[ix+(newCount-set->count)] = set->data[ix]; // We have to copy the old data across
    }
    free(set->data);
    set->data = newData;
    set->count = newCount;
    set->min = newBase;
    return addToSet(set, k);
  }

  assert(k>=set->min+set->count*64);
  int32 newCount = align64(k - set->min) / 64;
  uinteger *newData = (uinteger *) malloc(sizeof(uinteger) * newCount);
  for (int32 ix = 0; ix < set->count; ix++) {
    newData[ix] = set->data[ix];
  }
  for (int32 ix = set->count; ix < newCount; ix++) {
    newData[ix] = 0;
  }
  free(set->data);
  set->data = newData;
  set->count = newCount;
  return addToSet(set, k);
}

retCode removeFromSet(setPo set, int32 k) {
  if (set->data != Null) {
    int32 max = set->count * 64 + set->min;
    if (k < set->min || k >= max)
      return Error; // Never there
    int32 base = k - set->min;
    int32 el = base >> 6;
    uinteger mask = 1ul << ((uinteger) (base & 63u));
    set->data[el] &= ~mask;
    return Ok;
  }
  return Error;
}

logical setIsEmpty(setPo set) {
  if (set->data == Null)
    return True;
  else {
    for (int32 ix = 0; ix < set->count; ix++)
      if (set->data[ix] != 0)
        return False;
    return True;
  }
}

logical inSet(setPo set, int32 k) {
  int32 max = set->count * 64 + set->min;
  if (k < set->min || k >= max || set->data == Null)
    return False;
  int32 base = k - set->min;
  int32 el = base >> 6;
  uinteger mask = 1ul << (((uinteger) base) & 63u);
  return (set->data[el] & mask) == mask;
}

retCode processSet(setPo set, setElProc proc, void *cl) {
  if (set->data == Null)
    return Ok;
  retCode ret = Ok;
  int32 max = set->count * 64 + set->min;
  for (int32 ix = set->min; ret == Ok && ix < max; ix++) {
    int32 base = ix - set->min;
    int32 el = base >> 6;
    uinteger mask = 1ul << (((uinteger) base) & 63u);
    if ((set->data[el] & mask) == mask)
      ret = proc(set, ix, cl);
  }
  return ret;
}

typedef struct {
  ioPo out;
  logical showComma;
} showInfo;

static retCode showSetEl(setPo set, int32 el, void *cl) {
  showInfo *info = (showInfo *) cl;

  retCode ret = outMsg(info->out, "%s%d", (info->showComma ? "," : ""), el);
  info->showComma = True;
  return ret;
}

retCode showSet(ioPo out, setPo set) {
  if (set->data == Null)
    return outMsg(out, "∅");
  else {
    outMsg(out, "{");
    showInfo info = {.out = out, .showComma = False};
    retCode ret = processSet(set, showSetEl, &info);
    outMsg(out, "}%_");
    return ret;
  }
}
