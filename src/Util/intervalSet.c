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

static void initSets()
{
  if (setPool == Null){
    setPool = newPool(sizeof(SetRecord), 64);
    intervalPool = newPool(sizeof(IntervalRecord), 64);
  }
}

typedef retCode (*elProc)(int32 from, int32 to, void* cl);

static intervalPo deleteInterval(intervalPo intVal)
{
  if (intVal != Null){
    intervalPo prev = intVal->prev;
    intervalPo next = intVal->next;

    if (prev != intVal){
      prev->next = next;
      next->prev = prev;
      freePool(intervalPool, intVal);
      if (prev->from < next->from)
        return prev;
      else
        return next;
    }
    else{
      freePool(intervalPool, intVal);
      return Null;
    }
  }
  return Null;
}

intervalSetPo newISet()
{
  initSets();
  intervalSetPo set = allocPool(setPool);

  set->chain = Null;
  return set;
}

void deleteISet(intervalSetPo s)
{
  while (s->chain != Null){
    s->chain = deleteInterval(s->chain);
  }
  freePool(setPool, s);
}

static intervalPo coalesceLeft(intervalPo i)
{
  intervalPo left = i->prev;
  if (left->to == i->from){
    i->from = left->from;
    i->prev = left->prev;
    i->prev->next = i;
    freePool(intervalPool, left);
  }

  return i;
}

static intervalPo coalesceRight(intervalPo i)
{
  intervalPo right = i->next;
  if (i->to == right->from){
    i->to = right->to;
    i->next = right->next;
    right->next->prev = i;
    freePool(intervalPool, right);
  }
  return i;
}

static intervalPo addInterval(intervalPo after, int32 from, int32 to)
{
  intervalPo newInterval = allocPool(intervalPool);
  newInterval->from = from;
  newInterval->to = to;
  if (after != Null){
    assert(after->to<from);
    newInterval->prev = after;
    newInterval->next = after->next;
    after->next = newInterval;
    newInterval->next->prev = newInterval;
  }
  else
    newInterval->prev = newInterval->next = newInterval;
  return newInterval;
}

static intervalPo insertInterval(intervalPo before, int32 from, int32 to)
{
  intervalPo newInterval = allocPool(intervalPool);
  newInterval->from = from;
  newInterval->to = to;
  if (before != Null){
    assert(before->from>to);
    newInterval->next = before;
    newInterval->prev = before->prev;
    before->prev = newInterval;
    newInterval->prev->next = newInterval;
  }
  else
    newInterval->prev = newInterval->next = newInterval;
  return newInterval;
}

retCode addToISet(intervalSetPo set, int32 k)
{
  assert(set!=Null);
  intervalPo chain = set->chain;
  if (chain != Null){
    intervalPo examine = chain;

    do{
      if (examine->from <= k && k < examine->to){
        return Ok;
      }
      else if (examine->from == k + 1){
        coalesceLeft(examine);
        return Ok;
      }
      else if (examine->to + 1 == k){
        coalesceRight(examine);
        return Ok;
      }
        if (examine->to > k){
          insertInterval(examine, k, examine->to);
          return Ok;
        }
        else
          examine = examine->next;
      examine = examine->next;
    }
    while (examine != chain);

    assert(examine->to < k);
    addInterval(examine, k, k+1);
    return Ok;
  }

  set->chain = addInterval(Null, k, k+1);
  return Ok;
}

retCode removeFromISet(intervalSetPo set, int32 k);

logical inISet(intervalSetPo set, int32 k)
{
  assert(set!=Null);
  intervalPo chain = set->chain;
  if (chain != Null){
    intervalPo examine = chain;
    do{
      if (examine->from <= k && k < examine->to){
        return True;
      }
      examine = examine->next;
    }
    while (examine != chain);
    return False;
  }
  else
    return False;
}

logical isEmptyISet(intervalSetPo set)
{
  return set->chain == Null;
}

retCode processISet(intervalSetPo set, elProc proc, void* cl)
{
  assert(set!=Null);
  intervalPo chain = set->chain;
  if (chain != Null){
    intervalPo examine = chain;
    do{
      retCode ret = proc(examine->from, examine->to, cl);
      if (ret != Ok)
        return ret;
      examine = examine->next;
    }
    while (examine != chain);
    return Ok;
  }
  else
    return Ok;
}

static retCode showInterval(int32 from, int32 to, void* cl)
{
  ioPo out = (ioPo)cl;
  return outMsg(out, "[%d,%d)", from, to);
}

retCode showISet(ioPo out, intervalSetPo set)
{
  return processISet(set, showInterval, out);
}

static logical checkSet(intervalSetPo set)
{
  intervalPo chain = set->chain;
  if (chain != Null){
    logical inOrder = True;
    int32 from = chain->from;
    int32 to = chain->to;

    do{
      if (chain->from<chain->to){
        if (chain->from < from || chain->from <= to)
          return False;
        from = chain->from;
        to = chain->to;
        chain = chain->next;
      }
      else
        return False;
    } while (chain != set->chain);
  }
  return True;
}