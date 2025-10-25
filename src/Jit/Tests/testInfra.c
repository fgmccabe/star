//
// Created by Francis McCabe on 10/24/25.
//

#include "sortTests.h"
#include "ooio.h"

static logical usesReg(FlexOp op, mcRegister Rg) {
  switch (op.mode) {
    case reg:
    case offset:
      return op.reg == Rg;
    default:
      return False;
  }
}

logical affects(FlexOp src, FlexOp dst) {
  switch (src.mode) {
    case reg:
      return usesReg(dst, src.reg);
    case offset: {
      switch (dst.mode) {
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

retCode showFlexOp(ioPo f, void *data, long depth, long precision, logical alt) {
  FlexOp *flex = (FlexOp *) data;
  switch (flex->mode) {
    case reg:
      return outMsg(f, "R%d", flex->reg);
    case offset:
      return outMsg(f, "R%d[%d]", flex->reg, flex->immediate);
    default:
      return outMsg(f, "unknown flex op");
  }
}

void showGroups(ArgSpec defs[], int32 groups, int32 arity) {
  for (int32 gx = 0; gx < groups; gx++) {
    outMsg(logFile, "group %d: ", gx);
    char *sep = "";
    for (int32 ax = 0; ax < arity; ax++) {
      if (defs[ax].group == gx) {
        outMsg(logFile, "%sarg %F <- %F", sep, &defs[ax].dst, &defs[ax].src);
        sep = ", ";
      }
    }
    outStr(logFile, "\n");
  }
  flushOut();
}

void showDefs(ArgSpec defs[], int32 count) {
  char *sep = "";
  for (int32 ax = 0; ax < count; ax++) {
    outMsg(logFile, "%s%F <- %F", sep, &defs[ax].dst, &defs[ax].src);
    sep = ", ";
  }
  outStr(logFile, "\n");
  flushOut();
}

void collectGroup(argSpecPo args, int32 arity, int32 groupNo, argSpecPo *group) {
  int32 pt = 0;
  for (int32 ix = 0; ix < arity; ix++) {
    if (args[ix].group == groupNo) {
      group[pt++] = &args[ix];
    }
  }
}

logical checkUseBeforeOverride(argSpecPo specs, int32 count, int32 groups) {
  for (int32 gx = groups - 1; gx >= 0; gx--) {
    int32 gpCount = groupSize(specs, count, gx);
    argSpecPo gp[gpCount];
    collectGroup(specs, count, gx, gp);
    for (int32 ix = 0; ix < gpCount; ix++) {
      FlexOp dst = gp[ix]->dst;

      for (int32 gy = gx - 1; gy >= 0; gy--) {
        int32 gpyCount = groupSize(specs, count, gy);
        argSpecPo gpy[gpCount];
        collectGroup(specs, count, gy, gpy);
        for (int32 iy = 0; iy < gpyCount; iy++) {
          FlexOp src = gpy[iy]->src;
          if (affects(dst, src)) {
            return False;
          }
        }
      }
    }
  }
  return True;
}
