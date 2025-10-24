//
// Created by Francis McCabe on 10/24/25.
//

#include "sortTests.h"
#include "ooio.h"

static logical usesReg(FlexOp op, mcRegister Rg)
{
  switch (op.mode){
  case reg:
  case offset:
    return op.reg == Rg;
  default:
    return False;
  }
}

logical affects(FlexOp src, FlexOp dst)
{
  switch (src.mode){
  case reg:
    return usesReg(dst, src.reg);
  case offset:
    {
      switch (dst.mode){
      case reg:
        return dst.reg == src.reg;
      case offset:
        return dst.reg == src.reg && dst.immediate == src.immediate;
      default:
        return False;
      }
    }
  default:
    return False;
  }
}

void showGroups(ArgSpec defs[], int32 groups, int32 arity)
{
  for (int32 gx = 0; gx < groups; gx++){
    outMsg(logFile, "group %d: ", gx);
    char* sep = "";
    for (int32 ax = 0; ax < arity; ax++){
      if (defs[ax].group == gx){
        outMsg(logFile, "%sarg %F: %F", sep, defs[ax].dst, defs[ax].src);
        sep = ", ";
      }
    }
    outStr(logFile, "\n");
  }
  flushOut();
}

void showDefs(ArgSpec defs[], int32 arity)
{
  char* sep = "";
  for (int32 ax = 0; ax < arity; ax++){
    argSpecPo arg = &defs[ax];
    outMsg(logFile,
           "%sarg %F%s: %F",
           sep,
           arg->dst,
           (arg->mark ? "✓" : ""),
           arg->src);
    sep = ", ";
  }
  outStr(logFile, "\n");
  flushOut();
}

void collectGroup(argSpecPo args, int32 arity, int32 groupNo, argSpecPo* group)
{
  int32 pt = 0;
  for (int32 ix = 0; ix < arity; ix++){
    if (args[ix].group == groupNo){
      group[pt++] = &args[ix];
    }
  }
}
