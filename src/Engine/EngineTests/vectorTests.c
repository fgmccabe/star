//
// Created by Francis McCabe on 1/13/24.
//
#include "engineTests.h"
#include "vectorTests.h"
#include "vectP.h"
#include "engineP.h"
#include "charP.h"
#include "arith.h"
#include "formioP.h"
#include "formexts.h"

static termPo charTerm(heapPo h,integer ix, void*cl){
  char *str = (char*)cl;
  codePoint ch = (codePoint)str[ix];
  return allocateCharacter(ch);
}

static termPo vectorFromString(char *str){
  integer len = uniStrLen(str);
  return (termPo) makeVector(globalHeap,len,charTerm,(void*)str);
}

static retCode buildVectorTest() {
  char *testStr = "this is a string of 33 characters";
  normalPo v = C_NORMAL(vectorFromString(testStr));

  for(integer ix=0;ix< uniStrLen(testStr);ix++){
    termPo el = vectElement(v,ix);
    assert(el!=Null && isChar(el) && charVal(el)==testStr[ix]);
  }

  return Ok;
}


retCode vector_tests() {
  initHeap(200*1024);
  initLbls();
  initVect();
  initArith();
  initChars();
  installMsgProc('C', genQuotedChr);
  tryRet(run_test(buildVectorTest));
  return Ok;
}
