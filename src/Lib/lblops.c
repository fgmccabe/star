//
// Created by Francis McCabe on 3/11/18.
//


#include "lblops.h"

ReturnStatus g__defined(processPo P, ptrPo tos) {
  termPo Arg1 = tos[0];
  termPo Arg2 = tos[1];
  char label[MAX_SYMB_LEN];
  copyString2Buff(C_STR(Arg1), label, NumberOf(label));
  integer arity = integerVal(Arg2);

  labelPo lbl = findLbl(label, arity);
  ReturnStatus ret = {.ret=Ok, .rslt = findLbl(label, arity) != Null ? trueEnum : falseEnum};

  return ret;
}
