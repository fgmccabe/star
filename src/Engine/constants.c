//
// Created by Francis McCabe on 3/26/25.
//

#include "array.h"
#include "eitherP.h"
#include "constantsP.h"

typedef struct {
  int32 literalNo;
  termPo value;
} *constantPo, ConstantRecord;

static hashPo constantKeys = Null;
static arrayPo constants = Null;

void initConstants() {
  constantKeys = newHash(4096, (hashFun) termHash, (compFun) compTerm, Null);
  constants = allocArray(sizeof(ConstantRecord), 4096, True);
}

int32 constantLiteral(termPo t) {
  constantPo c = (constantPo) hashGet(constantKeys, t);
  if (c == Null)
    return -1;
  else
    return c->literalNo;
}

termPo getConstant(int32 key){
  if(key>=0 && key< arrayCount(constants)){
    constantPo c = nthEntry(constants,key);
    if(c!=Null)
      return c->value;
  }
  return Null;
}

int32 defineConstantLiteral(termPo t) {
  constantPo c = (constantPo) hashGet(constantKeys, t);
  if (c == Null) {
    c = (constantPo) newEntry(constants);
    c->literalNo = arrayCount(constants)-1;
    c->value = t;

    hashPut(constantKeys, t, c);
  }
  return c->literalNo;
}

retCode markConstant(void *entry, integer ix, void *cl) {
  gcSupportPo g = (gcSupportPo) cl;

  constantPo c = (constantPo) entry;
  c->value = markPtr(g, &c->value);

  return Ok;
}

void markConstants(gcSupportPo G) {
  processArrayElements(constants,markConstant,(void*)G);
}
