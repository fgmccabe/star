//
// Created by Francis McCabe on 1/13/24.
//

#include "heapP.h"
#include "stack.h"

retCode gcCollect(heapPo H, long amount) {
  syserr("no gc in tests");
  return Error;
}

termPo markPtr(gcSupportPo G, ptrPo p) {
  syserr("no gc in tests");
  return Null;
}

retCode jitMethod(methodPo mtd, char *errMsg, integer msgLen) {
  return Error;
}

retCode jitSpecialMethod(methodPo mtd, char* errMsg, integer msgLen) {
  return Error;
}


ssaInsPo showIns(ioPo out, methodPo mtd, stackPo stk, ssaInsPo pc) {
  return Null;
}

