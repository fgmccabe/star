//
// Created by Francis McCabe on 1/26/26.
//

#include "disass.h"
#include "codeP.h"
#include "constants.h"
#include "globals.h"
#include "stackP.h"

static ssaInsPo showOperands(ioPo out, methodPo mtd, ptrPo args, ssaInsPo pc, char *fmt);

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
#define none ""

  switch (pc->op.op) {
#define instr(Op, fmt) \
case s##Op:{\
  outMsg(out, #Op);\
  return showOperands(out,mtd,args,pc+1,fmt);\
}
#include "ssaInstructions.h"
    default:
      return 0;
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

static retCode showVarble(ioPo out, ptrPo args, int32 varNo);
static retCode showGlobal(ioPo out, globalPo glb);

ssaInsPo showOperands(ioPo out, methodPo mtd, ptrPo args, ssaInsPo pc, char *fmt) {
  ssaInsPo basePc = pc - 1;
  char *sep = " ";
  while (*fmt != '\0') {
    outMsg(out, sep);
    sep = ", ";
    switch (*fmt++) {
      case 's': // Program label
      case 'l': {
        termPo lit = getConstant(pc->op.ltrl);
        if (lit != Null) {
          outMsg(out, "%,*T", displayDepth, lit);
        } else {
          outMsg(out, "(unknown constant) [%%d]", lit);
        }
        pc++;
        continue;
      }
      case 'v': {
        // Variable reference
        showVarble(out, args, pc->op.ltrl);
        pc++;
        continue;
      }
      case 'V': {
        // Vector of variable references
        int32 count = pc->op.ltrl;
        pc++;
        outMsg(out, "[");
        char *argsep = "";
        for (int32 ax = 0; ax < count; ax++) {
          outMsg(out, argsep);
          argsep = ", ";
          showVarble(out, args, pc->op.ltrl);
          pc++;
        }
        outMsg(out, "]");
        continue;
      }
      case 'g': {
        // Global variable
        int32 glbNo = pc->op.ltrl;
        showGlobal(out, findGlobalVar(glbNo));
        pc++;
        continue;
      }
      case 'a': // Arity
      case 'i': {
        // Integer
        int32 ix = pc->op.ltrl;
        pc++;
        outMsg(out, "%d", ix);
        continue;
      }
      case 'e': {
        // Escape number
        int32 escno = pc->op.ltrl;
        pc++;
        outMsg(out, " %s", escapeName(getEscape(escno)));
        continue;
      }
      case 'k': {
        //  Vector of instructions
        int32 skip = pc->op.ltrl;
        pc++;
        int32 offset = codeOffset(mtd, basePc);
        outMsg(out, " ↓[%d]", skip + offset);
        continue;
      }
      case 'b': {
        int32 skip = pc->op.ltrl;
        int32 offset = codeOffset(mtd, basePc);

        pc++;
        outMsg(out, " ↑%d", offset + skip);
        continue;
      }
      default:
        syserr("unknown operand format");
        return Null;
    }
  }
  return pc;
}

retCode showVarble(ioPo out, ptrPo args, int32 varNo) {
  char *kind = (varNo >= 0 ? "A" : "L");
  if (args != Null) {
    termPo val = *stackVarble(args, varNo);
    if (val != voidEnum)
      return outMsg(out, "%s[%d] = %,*T", kind, varNo, displayDepth, val);
    else
      return outMsg(out, "%s[%d] (undef)", kind, varNo);
  }
  return outMsg(out, "%s[%d]", kind, varNo);
}

static retCode showGlobal(ioPo out, globalPo glb) {
  if (glb != Null) {
    if (glbIsSet(glb))
      return outMsg(out, " %s=%,*T", globalVarName(glb), displayDepth, getGlobal(glb));
    else
      return outMsg(out, " %s (undef)", globalVarName(glb));
  } else
    return outMsg(out, " unknown global");
}
