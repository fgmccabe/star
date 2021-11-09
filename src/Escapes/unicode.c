//
// Created by Francis McCabe on 3/3/18.
//

#include <arithP.h>
#include <globals.h>
#include <charP.h>
#include "unicode.h"
#include "char.h"

ReturnStatus g__uniCodeCategory(processPo p, heapPo h, termPo a1) {
  codePoint ch = (codePoint) charVal(a1);

  termPo Rs = (termPo) allocateInteger(h, uniCharCategory(ch));
  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCcChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCfChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCfChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCnChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCnChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCoChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCsChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isCsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLlChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLmChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLmChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLoChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLtChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLtChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLuChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLuChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMcChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isMcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMeChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isMeChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMnChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isMnChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNdChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isNdChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNlChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isNlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNoChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isNoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPcChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPdChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPdChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPeChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPeChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPfChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPfChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPiChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPiChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPoChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPsChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isPsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isScChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isScChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSkChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isSkChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSmChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isSmChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSoChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isSoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZlChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isZlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZpChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isZpChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZsChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isZsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLetterChar(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isLetterChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isIDStart(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isIdStart(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isIDContinue(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  termPo Rs = (isIdContinue(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__digitCode(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  if (isNdChar(ch)) {
    return (ReturnStatus) {.ret=Ok,
      .result=(termPo) allocateInteger(h, digitValue(ch))};

  } else {
    return (ReturnStatus) {.ret=Fail, .result=voidEnum};
  }
}

ReturnStatus g__codePoint(processPo p, heapPo h, termPo a1) {
  codePoint ch = charVal(a1);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateInteger(h, ch)};
}

ReturnStatus g__char(processPo p, heapPo h, termPo a1) {
  codePoint ch = integerVal(a1);

  return (ReturnStatus) {.ret=Ok,
    .result=(termPo) allocateCharacter(h, ch)};
}
