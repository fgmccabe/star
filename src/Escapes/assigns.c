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

  return (ReturnStatus){.ret=Ok, .result=(termPo) cell};
}

ReturnStatus g__get(processPo p, ptrPo tos) {
  cellPo cell = C_CELL(tos[0]);
  return (ReturnStatus){.ret=Ok, .result=getCell(cell)};
}

ReturnStatus g__assign(processPo p, ptrPo tos) {
  termPo Content = tos[1];
  cellPo Cell = C_CELL(tos[0]);

  setCell(Cell, Content);

  return (ReturnStatus){.ret=Ok, .result=(termPo) Content};
}

ReturnStatus g__overwrite(processPo p, ptrPo tos) {
  normalPo orig = C_TERM(tos[0]);
  normalPo newval = C_TERM(tos[1]);

  if (termArity(orig) == termArity(newval)) {
    orig->lbl = newval->lbl;
    for (integer ix = 0; ix < termArity(newval); ix++)
      orig->args[ix] = newval->args[ix];

    return (ReturnStatus){.ret=Ok, .result=(termPo)orig};
  } else {
    return (ReturnStatus){.ret=Error, .result= voidEnum};
  }
}

static globalPo globalLabel(termPo t) {
  char lblNm[MAX_SYMB_LEN];

  copyString2Buff(C_STR(t), lblNm, NumberOf(lblNm));

  return globalVar(lblNm, NULL);
}

ReturnStatus g__isDefinedVr(processPo p, ptrPo tos) {
  globalPo gv = globalLabel(tos[0]);

  return (ReturnStatus){.ret=Ok, .result=glbIsSet(gv) ? trueEnum : falseEnum};
}

ReturnStatus g__definedVr(processPo p, ptrPo tos) {
  globalPo gv = globalLabel(tos[0]);

  return (ReturnStatus){.ret=Ok, .result=getGlobal(gv)};
}

ReturnStatus g__defineVr(processPo p, ptrPo tos) {
  globalPo gv = globalLabel(tos[0]);
  termPo vl = tos[1];
  setGlobalVar(gv, vl);

  return (ReturnStatus){.ret=Ok, .result=trueEnum};
}
