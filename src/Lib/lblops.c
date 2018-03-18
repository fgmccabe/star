//
// Created by Francis McCabe on 3/11/18.
//


#include "lblops.h"

ReturnStatus g__defined(processPo P, ptrPo tos) {
  char label[MAX_SYMB_LEN];
  copyString2Buff(C_STR(tos[1]), label, NumberOf(label));
  integer arity = integerVal(tos[0]);

  labelPo lbl = findLbl(label, arity);
  ReturnStatus ret = {.ret=Ok, .rslt = findLbl(label, arity) != Null ? trueEnum : falseEnum};

  return ret;
}
