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

ValueReturn s__isCcChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCcChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isCfChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCfChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isCnChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCnChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isCoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCoChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isCsChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isCsChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isLlChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLlChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isLmChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLmChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isLoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLoChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isLtChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLtChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isLuChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLuChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isMcChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isMcChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isMeChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isMeChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isMnChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isMnChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isNdChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isNdChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isNlChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isNlChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isNoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isNoChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isPcChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPcChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isPdChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPdChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isPeChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPeChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isPfChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPfChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isPiChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPiChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isPoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPoChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isPsChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isPsChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isScChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isScChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isSkChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isSkChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isSmChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isSmChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isSoChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isSoChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isZlChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isZlChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isZpChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isZpChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isZsChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isZsChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__isLetterChar(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  return normalReturn(isLetterChar(ch) ? trueEnum : falseEnum);
}

ValueReturn s__digitCode(enginePo P, termPo c) {
  codePoint ch = charVal(c);

  if (isNdChar(ch)) {
    return normalReturn(makeInteger(digitValue(ch)));
  } else {
    return abnormalReturn(eINVAL);
  }
}

ValueReturn s__codePoint(enginePo P, termPo c) {
  codePoint ch = (codePoint) charVal(c);
  return normalReturn(makeInteger(ch));
}

ValueReturn s__int2chr(enginePo P, termPo c) {
  codePoint ch = (codePoint) integerVal(c);
  return normalReturn(makeChar(ch));
}
