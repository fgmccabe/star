//
// Created by Francis McCabe on 1/14/20.
//

#include "memoP.h"
#include <assert.h>
#include <globals.h>

static long memoSize(specialClassPo cl, termPo o);
static termPo memoCopy(specialClassPo cl, termPo dst, termPo src);
static termPo memoScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static logical memoCmp(specialClassPo cl, termPo o1, termPo o2);
static integer memoHash(specialClassPo cl, termPo o);
static retCode memoDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);

SpecialClass MemoClass = {
  .clss = Null,
  .sizeFun = memoSize,
  .copyFun = memoCopy,
  .scanFun = memoScan,
  .compFun = memoCmp,
  .hashFun = memoHash,
  .dispFun = memoDisp
};

clssPo memoClass = (clssPo) &MemoClass;

void initMemo() {
  MemoClass.clss = specialClass;
}

memoPo C_MEMO(termPo t) {
  assert(hasClass(t, memoClass));

  return (memoPo) t;
}

memoPo memoVar(heapPo H, normalPo provider) {
  memoPo memo = (memoPo) allocateObject(H, memoClass, MemoCellCount);

  memo->content = voidEnum;
  memo->provider = provider;
  memo->clss = memoClass;
  return memo;
}

long memoSize(specialClassPo cl, termPo o) {
  return MemoCellCount;
}

integer memoHash(specialClassPo cl, termPo o) {
  return (integer) o;
}

logical memoCmp(specialClassPo cl, termPo o1, termPo o2) {
  return (logical) (o1 == o2);

}

termPo memoCopy(specialClassPo cl, termPo dst, termPo src) {
  *((memoPo) dst) = *((memoPo) src);
  return dst + MemoCellCount;
}

termPo memoScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  memoPo memo = C_MEMO(o);

  helper((ptrPo) (&memo->provider), c);
  helper((ptrPo) (&memo->content), c);

  return o + MemoCellCount;
}

retCode memoDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  memoPo memo = C_MEMO(t);

  if (isMemoSet(memo))
    return outMsg(out, "[memo: 0x%x, %,*T]", memo, depth, memo->content);
  else
    return outMsg(out, "[memo 0x%x, unset]", memo);
}

logical isMemoSet(memoPo memo) {
  return (logical) (memo->content != voidEnum);
}

retCode setMemoValue(memoPo memo, termPo value) {
  if (isMemoSet(memo)) {
    if (sameTerm(memo->content, value))
      return Ok;
    else
      return Fail;
  } else {
    memo->content = value;
    return Ok;
  }
}

normalPo getMemoProvider(memoPo memo) {
  return memo->provider;
}

termPo getMemoContent(memoPo memo) {
  return memo->content;
}

