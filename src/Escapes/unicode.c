//
// Created by Francis McCabe on 3/3/18.
//

#include <arithP.h>
#include <charP.h>
#include "unicode.h"
#include "errorCodes.h"

ReturnStatus g__uniCodeCategory(enginePo P) {
  codePoint ch = (codePoint) charVal(popVal(P));

  pshVal(P, makeInteger(uniCharCategory(ch)));
  return Normal;
}

ReturnStatus g__isCcChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCcChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isCfChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCfChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isCnChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCnChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isCoChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isCsChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isCsChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLlChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLlChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLmChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLmChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLoChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLtChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLtChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLuChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLuChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isMcChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isMcChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isMeChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isMeChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isMnChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isMnChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isNdChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isNdChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isNlChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isNlChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isNoChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isNoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPcChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPcChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPdChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPdChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPeChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPeChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPfChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPfChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPiChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPiChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPoChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isPsChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isPsChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isScChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isScChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isSkChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isSkChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isSmChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isSmChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isSoChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isSoChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isZlChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isZlChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isZpChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isZpChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isZsChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isZsChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__isLetterChar(enginePo P) {
  codePoint ch = charVal(popVal(P));

  termPo Rs = (isLetterChar(ch) ? trueEnum : falseEnum);

  pshVal(P, Rs);
  return Normal;
}

ReturnStatus g__digitCode(enginePo P) {
  codePoint ch = charVal(popVal(P));

  if (isNdChar(ch)) {
    pshVal(P,makeInteger(digitValue(ch)));
    return Normal;
  } else {
    pshVal(P,eINVAL);
    return Abnormal;
  }
}

ValueReturn s__codePoint(enginePo P, termPo c){
  codePoint ch = (codePoint) charVal(c);
  return normalReturn(makeInteger(ch));
}

ReturnStatus g__codePoint(enginePo P) {
  ValueReturn ret = s__codePoint(P,popVal(P));
  pshVal(P,ret.value);
  return ret.status;
}


ValueReturn s__int2chr(enginePo P, termPo c){
  codePoint ch = (codePoint) integerVal(c);
  return normalReturn(makeChar(ch));
}

ReturnStatus g__int2chr(enginePo P) {
  ValueReturn ret = s__int2chr(P,popVal(P));
  pshVal(P,ret.value);
  return ret.status;
}

