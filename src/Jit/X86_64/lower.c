//
// Created by Francis McCabe on 7/9/20.
//
#include "jitP.h"
#include "jitOps.h"


retCode jit_Dup(insPo code,integer *pc,opAndSpec OpAnd,jitCompPo context){
  check(context->vTop>0 && context->vTop<NumberOf(context->vStack)-1, "stack out of bounds");
  context->vStack[context->vTop] = context->vStack[context->vTop-1];
  context->vTop++;
  return Ok;
}


