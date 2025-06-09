//
// Created by Francis McCabe on 3/3/18.
//

#include <arithP.h>
#include <charP.h>
#include "unicode.h"
#include "errorCodes.h"

ReturnStatus g__uniCodeCategory(processPo P) {
  codePoint ch = (codePoint) charVal(popVal(P));

  pshVal(P, makeInteger(uniCharCategory(ch)));
  return Normal;
}

ReturnStatus g__isCcChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCcChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isCfChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCfChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isCnChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCnChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isCoChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isCsChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCsChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLlChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLlChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLmChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLmChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLoChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLtChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLtChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLuChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLuChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isMcChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isMcChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isMeChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isMeChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isMnChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isMnChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isNdChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isNdChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isNlChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isNlChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isNoChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isNoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPcChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPcChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPdChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPdChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPeChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPeChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPfChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPfChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPiChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPiChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPoChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPsChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPsChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isScChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isScChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isSkChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isSkChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isSmChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isSmChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isSoChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isSoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isZlChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isZlChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isZpChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isZpChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isZsChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isZsChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLetterChar(processPo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLetterChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__digitCode(processPo P) {
  codePoint ch = charVal(popVal(P));

  if (isNdChar(ch)) {
    pshVal(P,makeInteger(digitValue(ch)));
    return Normal;
  } else {
    pshVal(P,eINVAL);
    return Abnormal;
  }
}

ReturnStatus g__codePoint(processPo P) {
  codePoint ch = charVal(popVal(P));
  pshVal(P, makeInteger(ch));
  return Normal;
}

ReturnStatus g__char(processPo P) {
  codePoint ch = (codePoint) integerVal(popVal(P));
  pshVal(P, allocateCharacter(ch));
  return Normal;
}
