//
// Created by Francis McCabe on 3/3/18.
//

#include <arithP.h>
#include <globals.h>
#include "unicode.h"
#include "arith.h"

ReturnStatus g__isCcChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isCcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCfChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isCfChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCnChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isCnChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCoChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isCoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isCsChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isCsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLlChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isLlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLmChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isLmChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLoChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isLoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLtChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isLtChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLuChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isLuChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMcChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isMcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMeChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isMeChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isMnChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isMnChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNdChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isNdChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNlChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isNlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isNoChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isNoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPcChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isPcChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPdChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isPdChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPeChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isPeChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPfChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isPfChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPiChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isPiChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPoChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isPoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isPsChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isPsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isScChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isScChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSkChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isSkChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSmChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isSmChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isSoChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isSoChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZlChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isZlChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZpChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isZpChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isZsChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isZsChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isLetterChar(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isLetterChar(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isIDStart(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isIdStart(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__isIDContinue(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  termPo Rs = (isIdContinue(ch) ? trueEnum : falseEnum);

  return (ReturnStatus) {.ret=Ok, .result=Rs};
}

ReturnStatus g__digitCode(processPo p, ptrPo tos) {
  codePoint ch = (codePoint) integerVal(tos[0]);

  if (isNdChar(ch)) {
    return (ReturnStatus) {.ret=Ok,
      .result=(termPo) allocateInteger(processHeap(p), digitValue(ch))};

  } else {
    return (ReturnStatus) {.ret=Fail, .result=voidEnum};
  }
}
