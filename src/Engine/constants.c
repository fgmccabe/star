//
// Created by Francis McCabe on 3/26/25.
//

#include "array.h"
#include "eitherP.h"
#include "constantsP.h"
#include "globals.h"

static hashPo constantKeys = Null;
static arrayPo constants = Null;

void initConstants() {
  constantKeys = newHash(4096, (hashFun) termHash, (compFun) compTerm, Null);
  constants = allocArray(sizeof(termPo), 4096, True);
  appendEntry(constants,&voidEnum);
}

termPo getConstant(int32 key){
  if(key>=0 && key< arrayCount(constants)){
    ptrPo c = nthEntry(constants,key);
    if(c!=Null)
      return *c;
  }
  return Null;
}

int32 defineConstantLiteral(termPo t) {
  integer tx = (integer) hashGet(constantKeys, t);
  if (tx==(integer)Null) {
    if(appendEntry(constants,&t)==Ok){
      integer cx = arrayCount(constants)-1;
    
      hashPut(constantKeys, t, (void*)cx);
      return (int32)cx;
    }
    else
      return -1;
  }
  return (int32)tx;
}

logical isDefinedConstant(int32 key){
  if(key>=0 && key< arrayCount(constants)){
    termPo constant = getConstant(key);
    return constant!=Null && (integer)hashGet(constantKeys, constant)==(integer)key;
  }
  else
    return False;
}

retCode markConstant(void *entry, integer ix, void *cl) {
  gcSupportPo g = (gcSupportPo) cl;

  ptrPo c = (ptrPo) entry;
  *c = markPtr(g, c);

  return Ok;
}

void markConstants(gcSupportPo G) {
  processArrayElements(constants,markConstant,(void*)G);
}
