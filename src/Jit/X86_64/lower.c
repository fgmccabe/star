//
// Created by Francis McCabe on 7/9/20.
//
#include "jitP.h"
#include "jitOps.h"

static int32 collectOperand(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

retCode jit_LdA(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop < NumberOf(context->vStack) - 1, "stack out of bounds");
  int32 lxlNo = collectOperand(code, pc);
  vOperand entry = {.loc=argument, .ix=lxlNo, .type=ptrTp, .litrl=Null};
  context->vStack[context->vTop] = entry;
  return Ok;
}

retCode jit_LdL(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop < NumberOf(context->vStack) - 1, "stack out of bounds");
  int32 lxlNo = collectOperand(code, pc);
  vOperand entry = {.loc=local, .ix=lxlNo, .type=ptrTp, .litrl=Null};
  context->vStack[context->vTop] = entry;
  return Ok;
}

retCode jit_Dup(insPo code, integer *pc, jitCompPo context) {
  check(context->vTop > 0 && context->vTop < NumberOf(context->vStack) - 1, "stack out of bounds");
  context->vStack[context->vTop] = context->vStack[context->vTop - 1];
  context->vTop++;
  return Ok;
}

retCode jit_Drop(insPo code,integer *c,jitCompPo context){
  check(context->vTop>0,"stack empty");
  context->vTop--;
  return Ok;
}

retCode jit_IAdd(insPo code,integer *pc,jitCompPo context){

}
//instruction(ISub,tOs,nOp,-1,"L R --> L-R")
//instruction(IMul,tOs,nOp,-1,"L R --> L*R")
//instruction(IDiv,tOs,nOp,-1,"L R --> L/R")
//instruction(IMod,tOs,nOp,-1,"L R --> L%R")
//instruction(IAbs,tOs,nOp,0,"L --> abs(L)")
