//
// Created by Francis McCabe on 10/30/23.
//
#include <assert.h>
#include "specials.h"
#include "termP.h"

static specialClassPo builtinClasses[specialCount];

void initSpecial(specialIndex ix,specialClassPo special){
  assert(ix>=0 && ix<specialCount);
  assert(builtinClasses[ix]==Null);

  builtinClasses[ix] = special;
}

logical isSpecialLbl(int32 ix){
  return ix<=0 && (-ix)<specialCount && builtinClasses[-ix]!=Null;
}

specialClassPo specialClassLbl(int32 ix){
  assert(isSpecialLbl(ix));
  return builtinClasses[-ix];
}
