//
// Created by Francis McCabe on 3/3/18.
//

#include <arithP.h>
#include <charP.h>
#include "unicode.h"
#include "errorCodes.h"

ValueReturn s__uniCodeCategory(enginePo P, termPo c) {
  codePoint ch = (codePoint) charVal(c);

  return normalReturn(makeInteger(uniCharCategory(ch)));
}

ReturnStatus g__uniCodeCategory(enginePo P) {
  ValueReturn ret = s__uniCodeCategory(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isCcChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCcChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isCcChar(enginePo P) {
  ValueReturn ret = s__isCcChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isCfChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCfChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isCfChar(enginePo P) {
  ValueReturn ret = s__isCfChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isCnChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCnChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isCnChar(enginePo P) {
  ValueReturn ret = s__isCnChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isCoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCoChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isCoChar(enginePo P) {
  ValueReturn ret = s__isCoChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isCsChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCsChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isCsChar(enginePo P) {
  ValueReturn ret = s__isCsChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isLlChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLlChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isLlChar(enginePo P) {
  ValueReturn ret = s__isLlChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isLmChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLmChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isLmChar(enginePo P) {
  ValueReturn ret = s__isLmChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isLoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLoChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isLoChar(enginePo P) {
  ValueReturn ret = s__isLoChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isLtChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLtChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isLtChar(enginePo P) {
  ValueReturn ret = s__isLtChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isLuChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLuChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isLuChar(enginePo P) {
  ValueReturn ret = s__isLuChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isMcChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isMcChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isMcChar(enginePo P) {
  ValueReturn ret = s__isMcChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isMeChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isMeChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isMeChar(enginePo P) {
  ValueReturn ret = s__isMeChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isMnChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isMnChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isMnChar(enginePo P) {
  ValueReturn ret = s__isMnChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isNdChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isNdChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isNdChar(enginePo P) {
  ValueReturn ret = s__isNdChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isNlChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isNlChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isNlChar(enginePo P) {
  ValueReturn ret = s__isNlChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isNoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isNoChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isNoChar(enginePo P) {
  ValueReturn ret = s__isNoChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isPcChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPcChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isPcChar(enginePo P) {
  ValueReturn ret = s__isPcChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isPdChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPdChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isPdChar(enginePo P) {
  ValueReturn ret = s__isPdChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isPeChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPeChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isPeChar(enginePo P) {
  ValueReturn ret = s__isPeChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isPfChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPfChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isPfChar(enginePo P) {
  ValueReturn ret = s__isPfChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isPiChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPiChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isPiChar(enginePo P) {
  ValueReturn ret = s__isPiChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isPoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPoChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isPoChar(enginePo P) {
  ValueReturn ret = s__isPoChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isPsChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPsChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isPsChar(enginePo P) {
  ValueReturn ret = s__isPsChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isScChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isScChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isScChar(enginePo P) {
  ValueReturn ret = s__isScChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isSkChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isSkChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isSkChar(enginePo P) {
  ValueReturn ret = s__isSkChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isSmChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isSmChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isSmChar(enginePo P) {
  ValueReturn ret = s__isSmChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isSoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isSoChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isSoChar(enginePo P) {
  ValueReturn ret = s__isSoChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isZlChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isZlChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isZlChar(enginePo P) {
  ValueReturn ret = s__isZlChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isZpChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isZpChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isZpChar(enginePo P) {
  ValueReturn ret = s__isZpChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isZsChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isZsChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isZsChar(enginePo P) {
  ValueReturn ret = s__isZsChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__isLetterChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLetterChar(ch) ? trueEnum : falseEnum);
}

ReturnStatus g__isLetterChar(enginePo P) {
  ValueReturn ret = s__isLetterChar(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__digitCode(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  if (isNdChar(ch)) {
    return normalReturn(makeInteger(digitValue(ch)));
  } else {
    return abnormalReturn(eINVAL);
  }
}

ReturnStatus g__digitCode(enginePo P) {
  termPo cc = popVal(P);
  ValueReturn ret = s__digitCode(P, cc);
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__codePoint(enginePo P, termPo c) {
  codePoint ch = (codePoint) charVal(c);
  return normalReturn(makeInteger(ch));
}

ReturnStatus g__codePoint(enginePo P) {
  ValueReturn ret = s__codePoint(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}

ValueReturn s__int2chr(enginePo P, termPo c) {
  codePoint ch = (codePoint) integerVal(c);
  return normalReturn(makeChar(ch));
}

ReturnStatus g__int2chr(enginePo P) {
  ValueReturn ret = s__int2chr(P, popVal(P));
  pshVal(P, ret.value);
  return ret.status;
}
