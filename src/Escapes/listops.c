//
// Created by Francis McCabe on 3/25/18.
//

#include <arithP.h>
#include <assert.h>
#include "listops.h"

ReturnStatus g__list_nil(processPo p, ptrPo tos) {
  integer cap = integerVal(tos[0]);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) createList(processHeap(p), cap)};
  return ret;
}

ReturnStatus g__list_empty(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);

  ReturnStatus ret = {.ret=Ok, .result=listSize(l) == 0 ? trueEnum : falseEnum};
  return ret;
}

ReturnStatus g__list_size(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) allocateInteger(processHeap(p), listSize(l))};
  return ret;
}

ReturnStatus g__list_nth(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  integer ix = integerVal(tos[1]);
  listPo l = C_LIST(Lhs);

  assert(ix >= 0 && ix < listSize(l));

  ReturnStatus ret = {.ret=Ok, .result=nthEl(l, ix)};
  return ret;
}

ReturnStatus g__list_append(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  listPo l = C_LIST(Lhs);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) appendToList(processHeap(p), l, Rhs)};
  return ret;
}

ReturnStatus g__list_prepend(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  listPo l = C_LIST(Lhs);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) prependToList(processHeap(p), l, Rhs)};
  return ret;
}

ReturnStatus g__list_slice(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);
  integer from = integerVal(tos[1]);
  integer count = integerVal(tos[2]);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) sliceList(processHeap(p), l, from, count)};
  return ret;
}

ReturnStatus g__list_front(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);
  integer count = integerVal(tos[1]);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) sliceList(processHeap(p), l, 0, count)};
  return ret;
}

ReturnStatus g__list_back(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);
  integer from = integerVal(tos[1]);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) sliceList(processHeap(p), l, from, listSize(l) - from)};
  return ret;
}

ReturnStatus g__list_concat(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  termPo Rhs = tos[1];
  listPo l = C_LIST(Lhs);
  listPo r = C_LIST(Rhs);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) concatList(processHeap(p), l, r)};
  return ret;
}

ReturnStatus g__list_reverse(processPo p, ptrPo tos) {
  termPo Lhs = tos[0];
  listPo l = C_LIST(Lhs);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) reverseList(processHeap(p), l)};
  return ret;
}

ReturnStatus g__list_flatten(processPo p, ptrPo tos) {
  listPo ll = C_LIST(tos[0]);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) flattenList(processHeap(p), ll)};
  return ret;
}

ReturnStatus g__list_insert(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);
  integer ix = integerVal(tos[1]);
  termPo vl = tos[2];

  ReturnStatus ret = {.ret=Ok, .result=(termPo) insertListEl(processHeap(p), l, ix, vl)};
  return ret;
}

ReturnStatus g__list_replace(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);
  integer ix = integerVal(tos[1]);
  termPo vl = tos[2];

  ReturnStatus ret = {.ret=Ok, .result=(termPo) replaceListEl(processHeap(p), l, ix, vl)};
  return ret;
}

ReturnStatus g__list_remove(processPo p, ptrPo tos) {
  listPo l = C_LIST(tos[0]);
  integer ix = integerVal(tos[1]);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) removeListEl(processHeap(p), l, ix)};
  return ret;
}
