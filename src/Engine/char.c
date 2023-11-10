//
// Created by Francis McCabe on 10/19/21.
//

#include "charP.h"
#include "assert.h"
#include "heapP.h"

static retCode chrDisp(ioPo out, termPo t, integer precision, integer depth, logical alt);
static integer chrHash(specialClassPo cl, termPo o);
static logical chrCmp(specialClassPo cl, termPo t1, termPo t2);

SpecialClass CharacterClass = {
  .clss = Null,
  .sizeFun = Null,
  .copyFun = Null,
  .scanFun = Null,
  .finalizer = Null,
  .compFun = chrCmp,
  .hashFun = chrHash,
  .dispFun = chrDisp
};

clssPo charClass = (clssPo) &CharacterClass;

void initChars() {
  CharacterClass.clss = specialClass;
  initSpecial(charLbl, &CharacterClass);
}

termPo allocateCharacter(codePoint cp) {
  return ((termPo) ((integer) ((cp << 2ul) | chrTg)));
}

logical chrCmp(specialClassPo cl, termPo t1, termPo t2) {
  codePoint ix1 = charVal(t1);
  codePoint ix2 = charVal(t2);

  return (logical) (ix1 == ix2);
}

integer chrHash(specialClassPo cl, termPo o) {
  return hash61(ptrPayload(o));
}

static retCode chrDisp(ioPo out, termPo t, integer precision, integer depth, logical alt) {
  codePoint cp = charVal(t);
  return outMsg(out, "`%C`", cp);
}

logical isChar(termPo t) {
  return pointerTag(t) == chrTg;
}

codePoint charVal(termPo o) {
#ifdef TRACEEXEC
  assert(isChar(o));
#endif
  return (codePoint) ptrPayload(o);
}
