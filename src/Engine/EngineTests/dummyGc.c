//
// Created by Francis McCabe on 1/13/24.
//

#include "heapP.h"

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

int32 jitPc(methodPo mtd, void *address) {
  return -1;
}
