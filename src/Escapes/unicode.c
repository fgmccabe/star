//
// Created by Francis McCabe on 3/3/18.
//

#include <arithP.h>
#include <globals.h>
#include <charP.h>
#include "unicode.h"
#include "char.h"

ReturnStatus g__uniCodeCategory(heapPo h, termPo a1) {
  codePoint ch = (codePoint) charVal(a1);

  termPo Rs = makeInteger(uniCharCategory(ch));
  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCcChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCfChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCfChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCnChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCnChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCoChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCsChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLlChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLmChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLmChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLoChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLtChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLtChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLuChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLuChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMcChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isMcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMeChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isMeChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMnChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isMnChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNdChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isNdChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNlChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isNlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNoChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isNoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPcChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPdChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPdChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPeChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPeChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPfChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPfChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPiChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPiChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPoChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPsChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isScChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isScChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSkChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isSkChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSmChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isSmChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSoChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isSoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZlChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isZlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZpChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isZpChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZsChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isZsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLetterChar(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLetterChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isIDStart(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isIdStart(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isIDContinue(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isIdContinue(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__digitCode(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  if (isNdChar(ch)) {
    return (ReturnStatus) {.ret=Ok, .result=makeInteger(digitValue(ch))};

  } else {
    return (ReturnStatus) {.ret=Fail, .result=voidEnum};
  }
}

ReturnStatus g__codePoint(heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  return (ReturnStatus) {.ret=Ok, .result= makeInteger(ch)};
}

ReturnStatus g__char(heapPo h, termPo a1) {
  codePoint ch = (codePoint)integerVal(a1);

  return (ReturnStatus) {.ret=Ok,
    .result=allocateCharacter(ch)};
}
