//
// Created by Francis McCabe on 3/25/18.
//

#include <arithP.h>
#include "listops.h"

ReturnStatus g__list_nil(processPo p, ptrPo tos) {
  integer cap = integerVal(tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) createList(processHeap(p), cap)};
  return ret;
}

ReturnStatus g__list_empty(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=listSize(l) == 0 ? trueEnum : falseEnum};
  return ret;
}

ReturnStatus g__list_size(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) allocateInteger(processHeap(p), listSize(l))};
  return ret;
}

ReturnStatus g__list_nth(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  listPo l = C_LIST(Lhs);

  ReturnStatus ret = {.ret=Ok, .rslt=nthEl(l, integerVal(Rhs))};
  return ret;
}

ReturnStatus g__list_append(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  listPo l = C_LIST(Lhs);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) appendToList(processHeap(p), l, Rhs)};
  return ret;
}

ReturnStatus g__list_prepend(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  listPo l = C_LIST(Lhs);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) prependToList(processHeap(p), l, Rhs)};
  return ret;
}

ReturnStatus g__list_slice(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);
  integer from = integerVal(tos[1]);
  integer count = integerVal(tos[2]);

  ReturnStatus ret = {.ret=Ok, .rslt=(termPo) sliceList(processHeap(p), l, from, count)};
  return ret;
}
