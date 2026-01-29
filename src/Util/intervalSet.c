//
// Created by Francis McCabe on 1/27/26.
//

#include <assert.h>

#include "pool.h"
#include "ooio.h"
#include "utils.h"

#include "intervalSetP.h"

static poolPo setPool = Null;
static poolPo intervalPool = Null;

static void initSets() {
  if (setPool == Null) {
    setPool = newPool(sizeof(SetRecord), 64);
    intervalPool = newPool(sizeof(IntervalRecord), 64);
  }
}

typedef retCode (*elProc)(int32 from, int32 to, void *cl);

intervalSetPo newIntervalSet() {
  initSets();
  intervalSetPo set = allocPool(setPool);

  set->chain = Null;
  return set;
}

void deleteIntervalSet(intervalSetPo s) {
  intervalPo chain = s->chain;

  while (chain != Null) {
    intervalPo next = chain->next;
    freePool(intervalPool, chain);
    chain = next;
  }
}

static intervalPo mergeIntervals(intervalPo a, intervalPo b) {
  if (a != Null && b != Null) {
    if (a->to == b->from) {
      a->to = b->to;
      a->next = b->next;
      freePool(intervalPool, b);
      return a;
    } else {
      a->next = b;
      return a;
    }
  }
  if (a != Null) {
    a->next = b;
    return a;
  }
  b->next = Null;
  return b;
}

static intervalPo addToInterval(intervalPo iv, int32 k) {
  if (iv == Null) {
    iv = allocPool(intervalPool);
    iv->from = k;
    iv->to = k + 1;
    iv->next = Null;
    return iv;
  } else if (iv->to <= k) {
    return mergeIntervals(iv, addToInterval(iv->next, k));
  } else if (iv->from <= k && k < iv->to) {
    return iv;
  } else {
    intervalPo newInterval = allocPool(intervalPool);
    newInterval->from = k;
    newInterval->to = k + 1;
    newInterval->next = iv;
    return newInterval;
  }
}

void addToIntervalSet(intervalSetPo set, int32 k) {
  assert(set!=Null);
  set->chain = addToInterval(set->chain, k);
}

static intervalPo removeFromInterval(intervalPo iv, int32 k) {
  if (iv == Null)
    return Null;
  else if (iv->to < k) {
    iv->next = removeFromInterval(iv->next, k);
    return iv;
  } else if (iv->from == k) {
    iv->from++;
    if (iv->from == iv->to) {
      intervalPo next = iv->next;
      freePool(intervalPool, iv);
      return next;
    } else
      return iv;
  } else if (iv->to == k + 1) {
    iv->to--;
    if (iv->from == iv->to) {
      intervalPo next = iv->next;
      freePool(intervalPool, iv);
      return next;
    } else
      return iv;
  } else if (iv->from <= k && k < iv->to) {
    intervalPo split = allocPool(intervalPool);
    split->from = k + 1;
    split->next = iv->next;
    iv->to = k;
    iv->next = split;
    return iv;
  } else {
    assert(k>=iv->to);
    return iv;
  }
}

void removeFromIntervalSet(intervalSetPo set, int32 k) {
  assert(set!=Null);
  set->chain = removeFromInterval(set->chain, k);
}

logical inIntervalSet(intervalSetPo set, int32 k) {
  assert(set!=Null);
  intervalPo chain = set->chain;

  while (chain != Null) {
    if (chain->from <= k && k < chain->to)
      return True;
    else if (chain->to <= k)
      chain = chain->next;
    else
      return False;
  }
  return False;
}

retCode findElement(intervalSetPo set, int32 from, int32 *i) {
  assert(set!=Null);

  intervalPo chain = set->chain;

  while (chain != Null) {
    if (chain->to > from) {
      *i = max(chain->from, from);
      return Ok;
    }
    chain = chain->next;
  }
  return Error;
}

int32 findSpace(intervalSetPo set, int32 from) {
  assert(set!=Null);

  intervalPo chain = set->chain;

  while (chain != Null) {
    if (chain->from <= from) {
      from = max(from,chain->to);
      chain = chain->next;
    } else
      return from;
  }
  return from;
}

logical intervalSetIsEmpty(intervalSetPo set) {
  return set->chain == Null;
}

retCode processIntervalSet(intervalSetPo set, elProc proc, void *cl) {
  assert(set!=Null);
  intervalPo chain = set->chain;

  while (chain != Null) {
    retCode ret = proc(chain->from, chain->to, cl);
    if (ret != Ok)
      return ret;
    chain = chain->next;
  }
  return Ok;
}

typedef struct {
  ioPo out;
  char *sep;
} ShowIntervalSetInfo;

static retCode showInterval(int32 from, int32 to, void *cl) {
  ShowIntervalSetInfo *info = (ShowIntervalSetInfo *) cl;

  retCode ret = outMsg(info->out, "%s[%d,%d)", info->sep, from, to);
  info->sep = ",";
  return ret;
}

retCode showIntervalSet(ioPo out, intervalSetPo set) {
  ShowIntervalSetInfo info = {.out = out, .sep = ""};
  outMsg(out, "{");
  retCode ret = processIntervalSet(set, showInterval, &info);
  outMsg(out, "}");
  return ret;
}

logical checkIntervalSet(intervalSetPo set) {
  intervalPo chain = set->chain;

  if (chain != Null) {
    int32 lastTo = chain->to;
    while (chain != Null) {
      if (chain->from >= chain->to || (chain != set->chain && chain->from < lastTo))
        return False;

      lastTo = chain->to;
      chain = chain->next;
    }
  }
  return True;
}
