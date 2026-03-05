//
// Created by Francis McCabe on 1/26/26.
//

#include "disass.h"
#include "codeP.h"
#include "stackP.h"

ssaInsPo disass(ioPo out, stackPo stk, methodPo mtd, ssaInsPo pc) {
  if (mtd != Null) {
    int32 offset = codeOffset(mtd, pc);
    labelPo lbl = mtdLabel(mtd);
    outMsg(out, "%,*T [%d] ", displayDepth, lbl, offset);
  } else {
    outMsg(out, "\?\?\? [%lx] ", pc);
  }
  ptrPo args = (stk != Null) ? stk->args : Null;

#undef instr
#define sym "s"
#define lcl "v"
#define lcls "V"
#define lit "l"
#define glb "g"
#define art "a"
#define i32 "i"
#define Es "e"
#define bLk "k"
#define lVl "b"
#define none

    switch (pc->op) {
#define instr(Op, fmt) \
case Op:{\
  return showOperands(out,args,pc,fmt);\
}
#include "ssaInstructions.h"
      default:
        return Null;
    }

#undef instr
#undef sym
#undef lcl
#undef lcls
#undef lit
#undef glb
#undef art
#undef i32
#undef Es
#undef bLk
#undef lVl
#undef lVls
#undef none
}
