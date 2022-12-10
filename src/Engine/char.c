//
// Created by Francis McCabe on 10/19/21.
//

#include "charP.h"
#include "assert.h"
#include "heapP.h"

#ifdef TRACEMEM
integer allocatedChars = 0;
#endif

static long chrSize(specialClassPo cl, termPo o);
static termPo chrCopy(specialClassPo cl, termPo dst, termPo src);
static termPo chrScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o);
static retCode chrDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static integer chrHash(specialClassPo cl, termPo o);
static logical chrCmp(specialClassPo cl, termPo t1, termPo t2);
static termPo chrFinalizer(specialClassPo class, termPo o);

SpecialClass CharacterClass = {
  .clss = Null,
  .sizeFun = chrSize,
  .copyFun = chrCopy,
  .scanFun = chrScan,
  .finalizer = chrFinalizer,
  .compFun = chrCmp,
  .hashFun = chrHash,
  .dispFun = chrDisp
};

clssPo charClass = (clssPo) &CharacterClass;

void initChars() {
  CharacterClass.clss = specialClass;
}

termPo allocateCharacter(heapPo H, codePoint cp) {
  charPo t = (charPo) allocateObject(H, charClass, CharCellCount);
  t->cp = cp;
#ifdef TRACEMEM
  if(traceAllocs)
    allocatedChars++;
#endif
  return (termPo) t;
}

long chrSize(specialClassPo cl, termPo o) {
  return CellCount(sizeof(CharacterRecord));
}

termPo chrCopy(specialClassPo cl, termPo dst, termPo src) {
  charPo si = C_CHAR(src);
  charPo di = (charPo) (dst);
  *di = *si;
  return (termPo) di + CharCellCount;
}

termPo chrScan(specialClassPo cl, specialHelperFun helper, void *c, termPo o) {
  return (termPo) (o + CharCellCount);
}

termPo chrFinalizer(specialClassPo class, termPo o) {
  return (termPo) (o + CharCellCount);
}

logical chrCmp(specialClassPo cl, termPo t1, termPo t2) {
  codePoint ix1 = charVal(t1);
  codePoint ix2 = charVal(t2);

  return (logical) (ix1 == ix2);
}

integer chrHash(specialClassPo cl, termPo o) {
  return hash61((C_CHAR(o))->cp);
}

static retCode chrDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  codePoint cp = charVal(t);
  return outMsg(out, "`%C`",cp);
}

charPo C_CHAR(termPo t) {
#ifdef TRACEEXEC
  assert(hasClass(t, charClass));
#endif
  return (charPo) t;
}

logical isChar(termPo t) {
  return
    hasClass(t, charClass);
}

codePoint charVal(termPo o) {
  charPo ix = C_CHAR(o);
  return ix->cp;
}
