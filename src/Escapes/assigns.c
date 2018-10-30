//
// Created by Francis McCabe on 3/5/18.
//

#include <arith.h>
#include <str.h>
#include <lblops.h>
#include <globals.h>
#include "assigns.h"
#include "cellP.h"

ReturnStatus g__cell(processPo p, ptrPo tos) {
  termPo content = tos[0];
  cellPo cell = newCell(processHeap(p), content);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) cell};

  return ret;
}

ReturnStatus g__get(processPo p, ptrPo tos) {
  cellPo cell = C_CELL(tos[0]);
  ReturnStatus ret = {.ret=Ok, .result=getCell(cell)};
  return ret;
}

ReturnStatus g__assign(processPo p, ptrPo tos) {
  termPo Content = tos[1];
  cellPo Cell = C_CELL(tos[0]);

  setCell(Cell, Content);

  ReturnStatus ret = {.ret=Ok, .result=(termPo) Content};

  return ret;
}

static globalPo globalLabel(termPo t) {
  char lblNm[MAX_SYMB_LEN];

  copyString2Buff(C_STR(t), lblNm, NumberOf(lblNm));

  return globalVar(lblNm);
}

ReturnStatus g__isDefinedVr(processPo p, ptrPo tos) {
  globalPo gv = globalLabel(tos[0]);

  ReturnStatus ret = {.ret=Ok, .result=glbIsSet(gv) ? trueEnum : falseEnum};

  return ret;
}

ReturnStatus g__definedVr(processPo p, ptrPo tos) {
  globalPo gv = globalLabel(tos[0]);

  ReturnStatus ret = {.ret=Ok, .result=getGlobal(gv)};

  return ret;
}

ReturnStatus g__defineVr(processPo p, ptrPo tos) {
  globalPo gv = globalLabel(tos[0]);
  termPo vl = tos[1];
  setGlobalVar(gv, vl);

  ReturnStatus ret = {.ret=Ok, .result=trueEnum};

  return ret;
}
