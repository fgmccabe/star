//
// Created by Francis McCabe on 3/11/18.
//


#include "turm.h"

logical sameTerm(termPo t1, termPo t2) {
  return (logical) (compareTerm(t1, t2) == same);
}

ReturnStatus g__identical(processPo P, ptrPo tos) {
  ReturnStatus rt = {.ret=Ok, .result=(sameTerm(tos[1], tos[0]) ? trueEnum : falseEnum)};
  return rt;
}
