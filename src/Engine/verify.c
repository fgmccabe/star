//
// Created by Francis McCabe on 7/26/18.
//

#include <stdlib.h>
#include <globals.h>
#include <debug.h>
#include "verifyP.h"
#include "topSort.h"

logical enableVerify = True;         // True if we verify code as it is loaded
logical traceVerify = False;      // true if tracing code verification

static segPo findSeg(vectorPo blocks, integer pc);
static segPo splitSeg(vectorPo blocks, integer tgt);
static varPo copyVars(varPo src, integer count);
static retCode mergeSegVars(segPo seg, segPo next);
static retCode checkSegment(segPo seg, char *errorMsg, long msgLen);
static void showGroup(vectorPo group, integer groupNo);
static void showGroups(vectorPo groups, char *name);
static int32 collect32(insPo base, integer *pc);

static integer segmentHash(objectPo o);

static logical segmentEquality(objectPo o1, objectPo o2);

static void segmentInit(objectPo o, va_list *args);

static void segmentDestroy(objectPo o);

SegmentClassRec SegmentClass = {
  {
    (classPo) &ObjectClass,
    "segment",
    O_INHERIT_DEF,
    O_INHERIT_DEF,
    segmentDestroy,
    O_INHERIT_DEF,
    segmentInit,
    sizeof(SegmentRecord),
    segmentHash,
    segmentEquality,
    NULL,
    PTHREAD_ONCE_INIT,                      /* not yet initialized */
    PTHREAD_MUTEX_INITIALIZER
  },
  {}
};

classPo segmentClass = (classPo) &SegmentClass;

void segmentInit(objectPo o, va_list *args) {
  segPo s = O_SEG(o);

  s->seg.segNo = va_arg(*args, int);
  s->seg.mtd = va_arg(*args, methodPo);
  s->seg.arity = codeArity(s->seg.mtd);
  s->seg.pc = va_arg(*args, integer);
  s->seg.maxPc = va_arg(*args, integer);
  s->seg.entryPoints = 0;
  s->seg.checked = False;
  s->seg.lclCount = lclCount(s->seg.mtd);
  s->seg.entries = vector(0);
  s->seg.exits = vector(0);
  s->seg.stackDepth = va_arg(*args, integer);

  logical initVars = va_arg(*args, logical);

  if (initVars) {
    s->seg.args = (varPo) malloc(sizeof(Var) * s->seg.arity);
    s->seg.locals = (varPo) malloc(sizeof(Var) * s->seg.lclCount);

    for (integer ix = 0; ix < s->seg.arity; ix++) {
      varPo a = &s->seg.args[ix];
      a->inited = True;
      a->read = False;
    }

    for (integer ix = 0; ix < s->seg.lclCount; ix++) {
      varPo l = &s->seg.locals[ix];
      l->inited = False;
      l->read = False;
    }
  } else {
    s->seg.args = Null;
    s->seg.locals = Null;
  }
}

varPo copyVars(varPo src, integer count) {
  varPo vars = (varPo) malloc(sizeof(Var) * count);
  for (integer ix = 0; ix < count; ix++)
    vars[ix] = src[ix];
  return vars;
}

segPo newSegment(int segNo, methodPo mtd, integer pc, integer maxPc, integer stackDepth, logical initVars) {
  return (segPo) newObject(segmentClass, segNo, mtd, pc, maxPc, stackDepth, initVars);
}

void segmentDestroy(objectPo o) {
  segPo v = O_SEG(o);

  if (v->seg.args != Null)
    free(v->seg.args);
  v->seg.args = Null;

  if (v->seg.locals != Null)
    free(v->seg.locals);
  v->seg.locals = Null;

  decReference(O_OBJECT(v->seg.exits));
  decReference(O_OBJECT(v->seg.entries));
}

static integer segmentHash(objectPo o) {
  segPo s = O_SEG(o);
  return s->seg.segNo;
}

static logical segmentEquality(objectPo o1, objectPo o2) {
  segPo s1 = O_SEG(o1);
  segPo s2 = O_SEG(o2);
  return (logical) (s1->seg.segNo == s2->seg.segNo);
}

static retCode
splitIns(vectorPo blocks, insPo code, integer *pc, integer to, logical jmpSplit, char *errorMsg, long msgLen);

static retCode findTgts(vectorPo blocks, insPo code, integer *pc, integer to, char *errorMsg, long msgLen);

static vectorPo getRefs(objectPo def, void *cl) {
  segPo seg = O_SEG(def);
  return seg->seg.entries;
}

retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen) {
  vectorPo blocks = vector(0);
  segPo first = newSegment(0, mtd, 0, insCount(mtd), 0, True);

  appendVectEl(blocks, O_OBJECT(first));

  integer pc = 0;
  retCode ret = splitIns(blocks, entryPoint(mtd), &pc, insCount(mtd), True, errorMsg, msgLen);

  if (ret == Ok) {
    pc = 0;
    ret = findTgts(blocks, entryPoint(mtd), &pc, insCount(mtd), errorMsg, msgLen);
  }

  vectorPo groups = topSort(blocks, getRefs, Null);

#ifdef TRACEVERIFY
  if (traceVerify) {
    showMethodCode(logFile, name, mtd);
    showGroups(groups, name);
    flushOut();
  }
#endif

  if (ret == Ok) {
    for (integer gx = 0; ret == Ok && gx < vectLength(groups); gx++) {
      vectorPo group = O_VECT(getVectEl(groups, gx));

#ifdef TRACEVERIFY
      if (traceVerify) {
        showGroup(group, gx);
        flushOut();
      }
#endif

      // Find out if any elements of the group have been entered
      logical done = False;
      while (!done && ret == Ok) {
        done = True;
        for (integer sx = 0; ret == Ok && sx < vectLength(group); sx++) {
          segPo seg = O_SEG(getVectEl(group, sx));
          if (!seg->seg.checked && seg->seg.args != Null) {
            done = False;
            char eMsg[MAXLINE];
            ret = checkSegment(seg, eMsg, NumberOf(eMsg));
            if (ret != Ok) {
              strMsg(errorMsg, msgLen, " %s in %M", eMsg, mtd);
              break;
            }
          }
        }
      }
    }

#ifdef TRACEVERIFY
    if (traceVerify)
      showGroups(groups, name);
#endif

    if (ret == Ok) {
      for (integer gx = 0; ret == Ok && gx < vectLength(groups); gx++) {
        vectorPo group = O_VECT(getVectEl(groups, gx));

        for (integer sx = 0; sx < vectLength(group); sx++) {
          segPo seg = O_SEG(getVectEl(group, sx));
          if (!seg->seg.checked) {
            strMsg(errorMsg, msgLen, RED_ESC_ON "unreachable segment %d @ PC:%d" RED_ESC_OFF, seg->seg.segNo,
                   seg->seg.pc);
            ret = Error;
            break;
          }
        }
      }
    }
  }

#ifdef TRACEVERIFY
  assert(referenceCount(O_OBJECT(blocks)) == 1);
  assert(referenceCount(O_OBJECT(groups)) == 1);
#endif

  decReference(O_OBJECT(blocks));
  decReference(O_OBJECT(groups));

  return ret;
}

segPo findSeg(vectorPo blocks, integer pc) {
  for (integer ix = 0; ix < vectLength(blocks); ix++) {
    segPo seg = O_SEG(getVectEl(blocks, ix));
    assert(seg != Null);
    if (seg->seg.pc <= pc && pc < seg->seg.maxPc)
      return seg;
  }
  return Null;
}


// Phase 1: split code into basic blocks

#undef instruction
#define instruction(Op, A1, Delta, Cmt)\
    case Op:\
      ret=checkSplit(blocks,code,oPc,pc,Op,A1,jmpSplit,errorMsg,msgLen);\
      continue;

static retCode
checkSplit(vectorPo blocks, insPo code, integer oPc, integer *pc, OpCode op, opAndSpec A, logical jmpSplit,
           char *errorMsg, long msgLen);

retCode
splitIns(vectorPo blocks, insPo code, integer *pc, integer to, logical jmpSplit, char *errorMsg, long msgLen) {
  retCode ret = Ok;
  while (ret == Ok && *pc < to) {
    integer oPc = *pc;
    switch (code[(*pc)++]) {
#include "instructions.h"

      default:
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid instruction at %d" RED_ESC_OFF, pc);
        return Error;
    }
  }
  return ret;
}

retCode checkSplit(vectorPo blocks, insPo code, integer oPc, integer *pc, OpCode op, opAndSpec A, logical jmpSplit,
                   char *errorMsg, long msgLen) {
  retCode ret = Ok;

  switch (A) {
    case nOp:                                   // No operand
    case tOs:
      break;
    case i32:          /* 32 bit literal operand */
    case art:          // Arity
    case arg:          /* argument variable offset */
    case lcl:          /* local variable offset */
    case lcs: {        // Store to local variable
      collect32(code, pc);
      break;
    }
    case off: {         /* offset within current code */
      integer delta = collect32(code, pc);
      integer nPc = *pc + delta;
      splitSeg(blocks, nPc);
      break;
    }
    case Es:
    case lit:          /* constant literal */
    case lne:           // Location literal
    case sym:           // Symbol
    case glb:           // Global variable name
      collect32(code, pc);
      return Ok;
    default:
      strMsg(errorMsg, msgLen, RED_ESC_ON "invalid operand specifier %d @ %d" RED_ESC_OFF, A, pc);
      return Error;
  }

  if (jmpSplit) {
    switch (op) {
      case Tail:
      case OTail:
      case Jmp:
      case Halt:
      case Ret: {
        splitSeg(blocks, *pc);
        return Ok;
      }
      case Bf:
      case Bt:
      case Cmp:
      case CLbl:
      case CV:
      case LM: {
        splitSeg(blocks, *pc);
        return Ok;
      }

      case Case: {
        integer iPc = oPc + 1;
        int32 caseCnt = collect32(code, &iPc) + 1;

        return splitIns(blocks, code, pc, *pc + caseCnt * 3, False, errorMsg, msgLen);
      }

      default:;
    }
  }

  return ret;
}

segPo splitSeg(vectorPo blocks, integer tgt) {
  segPo seg = findSeg(blocks, tgt);

  if (seg != Null && tgt != seg->seg.pc) {
    segPo new = newSegment((int) vectLength(blocks), seg->seg.mtd, tgt, seg->seg.maxPc, seg->seg.stackDepth, False);

    seg->seg.maxPc = tgt;

    appendVectEl(blocks, O_OBJECT(new));

    return new;
  } else
    return seg;
}

// Phase 2: wire up basic block targets

#undef instruction
#define instruction(Op, A1, Delta, Cmt)\
    case Op:\
      ret=checkTgt(blocks,code,oPc,pc,Op,A1,(*pc)>=to,errorMsg,msgLen);\
      break;

static retCode
checkTgt(vectorPo blocks, insPo code, integer oPc, integer *pc, OpCode op, opAndSpec A, logical isLast,
         char *errorMsg, long msgLen);

static retCode checkDest(vectorPo blocks, integer pc, integer tgt, char *errorMsg, long msgLen);

retCode findTgts(vectorPo blocks, insPo code, integer *pc, integer to, char *errorMsg, long msgLen) {
  retCode ret = Ok;
  while (ret == Ok && *pc < to) {
    integer oPc = *pc;
    insWord ins = code[(*pc)++];
    switch (ins) {
#include "instructions.h"

      default:
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid instruction at %d" RED_ESC_OFF, pc);
        return Error;
    }

    switch (ins) {
      case Bf:
      case Bt:
      case Cmp:
      case CLbl:
      case CV:
      case LM: {
        ret = checkDest(blocks, oPc, *pc, errorMsg, msgLen);
        break;
      }
      default:;
    }
  }

  return ret;
}

retCode checkDest(vectorPo blocks, integer pc, integer tgt, char *errorMsg, long msgLen) {
  segPo tgtSeg = findSeg(blocks, tgt);

  if (tgtSeg != Null) {
    segPo srcSeg = findSeg(blocks, pc);

    if (srcSeg != Null) {
      tgtSeg->seg.entryPoints++;
      appendVectEl(tgtSeg->seg.entries, O_OBJECT(srcSeg));
      appendVectEl(srcSeg->seg.exits, O_OBJECT(tgtSeg));
      return Ok;
    } else {
      strMsg(errorMsg, msgLen, RED_ESC_ON "invalid source pc %d" RED_ESC_OFF, pc);
      return Error;
    }
  } else {
    strMsg(errorMsg, msgLen, RED_ESC_ON "invalid target pc %d @ %d" RED_ESC_OFF, tgt, pc);
    return Error;
  }
}

retCode checkTgt(vectorPo blocks, insPo code, integer oPc, integer *pc, OpCode op, opAndSpec A, logical isLast,
                 char *errorMsg, long msgLen) {
  retCode ret = Ok;

  switch (A) {
    case nOp:                                   // No operand
    case tOs:
      break;
    case i32:          /* 32 bit literal operand */
    case art:          // Arity
    case arg:          /* argument variable offset */
    case lcl:          /* local variable offset */
    case lcs: {        // Store to local variable
      collect32(code, pc);
      break;
    }
    case off: {         /* offset within current code */
      integer delta = collect32(code, pc);
      integer nPc = *pc + delta;
      ret = checkDest(blocks, oPc, nPc, errorMsg, msgLen);
      break;
    }
    case Es:
    case lit:          /* constant literal */
    case sym:           // Symbol
    case lne:           // Location literal
    case glb:           // Global variable name
      collect32(code, pc);
      break;
    default:
      strMsg(errorMsg, msgLen, RED_ESC_ON "invalid operand specifier %d @ %d" RED_ESC_OFF, A, pc);
      return Error;
  }
//  if (isLast) {
//    segPo next = findSeg(blocks, *pc);
//    segPo current = findSeg(blocks, oPc);
//
//    if (next != Null && current != Null) {
//      switch (code[oPc]) {
//        case Jmp:
//        case Tail:
//        case OTail:
//        case Throw:
//        case Unwind:
//          break;
//        default:
//          appendVectEl(next->seg.entries, O_OBJECT(newInteger(current->seg.segNo)));
//          appendVectEl(current->seg.exits, O_OBJECT(newInteger(next->seg.segNo)));
//      }
//    }
//  }
  return ret;
}

// Phase 3: verify instructions are behaving

retCode mergeSegVars(segPo seg, segPo next) {
  if (next->seg.locals == Null) {
    next->seg.locals = copyVars(seg->seg.locals, seg->seg.lclCount);
    next->seg.args = copyVars(seg->seg.args, seg->seg.arity);
    next->seg.stackDepth = seg->seg.stackDepth;
  } else {
    assert(next->seg.locals != Null && seg->seg.locals != Null && next->seg.lclCount == seg->seg.lclCount);
    for (integer i = 0; i < next->seg.lclCount; i++) {
      next->seg.locals[i].inited &= seg->seg.locals[i].inited;
      next->seg.locals[i].read |= seg->seg.locals[i].read;
    }

    assert(next->seg.args != Null && seg->seg.args != Null && next->seg.arity == seg->seg.arity);
    for (integer i = 0; i < seg->seg.arity; i++) {
      next->seg.args[i].inited &= seg->seg.args[i].inited;
      next->seg.args[i].read |= seg->seg.args[i].read;
    }

    next->seg.stackDepth = minimum(next->seg.stackDepth, seg->seg.stackDepth);
  }

  return Ok;
}

int32 collect32(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

static retCode
checkInstruction(segPo seg, OpCode op, integer oPc, integer *pc, opAndSpec A1, integer delta, char *errorMsg,
                 long msgLen);

static retCode checkOperand(segPo seg, integer oPc, integer *pc, opAndSpec A, char *errorMsg, long msgLen) {
  insPo base = entryPoint(seg->seg.mtd);

  switch (A) {
    case nOp:                                   // No operand
      return Ok;
    case tOs:
      return Ok;
    case art:
    case i32: {                     /* 32 bit literal operand */
      collect32(base, pc);
      return Ok;
    }
    case arg: {                     /* argument variable access */
      int32 argNo = collect32(base, pc);
      if (argNo >= 0 && argNo < seg->seg.arity) {
        if (seg->seg.args[argNo].inited) {
          seg->seg.args[argNo].read = True;
          return Ok;
        } else {
          strMsg(errorMsg, msgLen, RED_ESC_ON "access to uninitialized argument %d @ %d" RED_ESC_OFF, argNo + 1, *pc);
          return Error;
        }
      } else {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid argument number %d @ %d" RED_ESC_OFF, argNo, *pc);
        return Error;
      }
    }
    case lcl: {                       /* local variable access */
      int32 lclVr = collect32(base, pc) - 1;
      if (lclVr >= 0 && lclVr < seg->seg.lclCount) {
        if (seg->seg.locals[lclVr].inited) {
          seg->seg.locals[lclVr].read = True;
          return Ok;
        } else {
          strMsg(errorMsg, msgLen, RED_ESC_ON " access to uninitialized local var %d @ %d" RED_ESC_OFF, lclVr + 1, oPc);
          return Error;
        }
      } else {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid local var number %d @ %d" RED_ESC_OFF, lclVr + 1, oPc);
        return Error;
      }
    }
    case lcs: {                        // Store to local variable
      int32 lclVr = collect32(base, pc) - 1;
      if (lclVr >= 0 && lclVr < seg->seg.lclCount) {
        seg->seg.locals[lclVr].inited = True;
        seg->seg.locals[lclVr].read = False;
        return Ok;
      } else {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid local var target %d @ %d" RED_ESC_OFF, lclVr + 1, *pc);
        return Error;
      }
    }
    case off: {                         /* offset within current code */
      int32 delta = collect32(base, pc);
      integer npc = *pc + delta;
      segPo alt = findSeg(seg->seg.exits, npc);

      if (alt == Null || alt->seg.pc != npc) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid target of branch: %d @ %d" RED_ESC_OFF, npc, *pc);
        return Error;
      } else
        return mergeSegVars(seg, alt);
    }
    case Es: {                          // escape code
      int32 escNo = collect32(base, pc);

      if (getEscape(escNo) == Null) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid escape number: %d @ %d" RED_ESC_OFF, escNo, *pc);
        return Error;
      }
      return Ok;
    }
    case lne:
    case sym:
    case lit: {                          /* constant literal */
      int32 litNo = collect32(base, pc);
      if (litNo < 0 || litNo >= codeLitCount(seg->seg.mtd)) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid literal number: %d @ %d" RED_ESC_OFF, litNo, *pc);
        return Error;
      }

      return Ok;
    }
    case glb: {                          // Global variable name
      int32 glbNo = collect32(base, pc);

      if (!isValidGlobalVarNo(glbNo)) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid global variable number: %d @ %d" RED_ESC_OFF, glbNo, *pc);
        return Error;
      }
      return Ok;
    }
    default:
      strMsg(errorMsg, msgLen, RED_ESC_ON "invalid instruction @ %d" RED_ESC_OFF, *pc);
      return Error;
  }
}

retCode
checkInstruction(segPo seg, OpCode op, integer oPc, integer *pc, opAndSpec A1, integer delta, char *errorMsg,
                 long msgLen) {
  retCode ret = checkOperand(seg, oPc, pc, A1, errorMsg, msgLen);

  if (ret == Ok) {
    seg->seg.stackDepth += delta;
    integer iPc = oPc + 1;
    // Special handling for specific instructions
    switch (op) {
      case Call: {
        int32 litNo = collect32(entryPoint(seg->seg.mtd), &iPc);
        if (litNo < 0 || litNo >= codeLitCount(seg->seg.mtd)) {
          strMsg(errorMsg, msgLen, RED_ESC_ON "invalid literal number: %d @ %d" RED_ESC_OFF, litNo, oPc);
          return Error;
        }
        termPo lit = getMtdLit(seg->seg.mtd, litNo);
        if (isALabel(lit)) {
          integer arity = labelArity(C_LBL(lit));
          if (seg->seg.stackDepth < arity) {
            strMsg(errorMsg, msgLen, RED_ESC_ON "insufficient args on stack: %d @ %d" RED_ESC_OFF, arity, oPc);
            return Error;
          }
          seg->seg.stackDepth -= arity - 1;
        } else {
          strMsg(errorMsg, msgLen, RED_ESC_ON "invalid call label: %t @ %d" RED_ESC_OFF, lit, oPc);
          return Error;
        }
        break;
      }
      case Tail:
        seg->seg.stackDepth = 0;
        break;
      case Rst: {
        int32 depth = collect32(entryPoint(seg->seg.mtd), &iPc);
        seg->seg.stackDepth = depth;
        break;
      }
      case Bf:
      case Bt:
      case Cmp:
      case CLbl:
      case CV:
      case LM: {
        segPo alt = findSeg(seg->seg.exits, *pc);

        if (alt == Null || alt->seg.pc != *pc) {
          strMsg(errorMsg, msgLen, RED_ESC_ON "invalid target of branch: %d @ %d" RED_ESC_OFF, *pc, oPc);
          return Error;
        } else
          return mergeSegVars(seg, alt);
      }

      default:;
    }
  }

  return ret;
}

#undef instruction
#define instruction(Mn, A1, Dlta, Cmt)\
  case Mn:\
    ret = checkInstruction(seg,Mn,oPc,&pc,A1,Dlta,errorMsg,msgLen);\
    continue;

static void showSeg(segPo seg);

retCode checkSegment(segPo seg, char *errorMsg, long msgLen) {
  if (!seg->seg.checked) {
    integer pc = seg->seg.pc;
    integer limit = seg->seg.maxPc;
    retCode ret = Ok;

    if (traceVerify) {
      outMsg(logFile, "On entry: ");
      showSeg(seg);
    }

    seg->seg.checked = True;
    insPo base = entryPoint(seg->seg.mtd);

    while (ret == Ok && pc < limit) {
      integer oPc = pc;
      switch (base[pc++]) {
#include "instructions.h"

        default:
          strMsg(errorMsg, msgLen, RED_ESC_ON "illegal instruction at %d" RED_ESC_OFF, pc);
          return Error;
      }
    }
#ifdef TRACEVERIFY
    if (traceVerify) {
      if (ret != Ok)
        outMsg(logFile, "Error: block: %d, %s\n", seg->seg.segNo, errorMsg);
      outMsg(logFile, "On exit:  ");
      showSeg(seg);
    }
#endif
    return ret;
  }
  return Ok;
}

static void showVar(char *nm, integer ix, varPo v) {
  outMsg(logFile, " %s[%d]%s%s", nm, ix, v->inited ? "*" : "", v->read ? "R" : "");
}

void showSeg(segPo seg) {
  integer i;

  outMsg(logFile, "segment:%s %d, (%d entrypoints) [%d-%d](%d) depth=%d",
         seg->seg.checked ? "¶" : "",
         seg->seg.segNo,
         seg->seg.entryPoints, seg->seg.pc, seg->seg.maxPc, seg->seg.maxPc - seg->seg.pc,
         seg->seg.stackDepth);

  for (integer ix = 0; ix < vectLength(seg->seg.entries); ix++) {
    segPo entry = O_SEG(getVectEl(seg->seg.entries, ix));
    outMsg(logFile, ", src=%d", entry->seg.segNo);
  }

  for (integer ix = 0; ix < vectLength(seg->seg.exits); ix++) {
    segPo entry = O_SEG(getVectEl(seg->seg.exits, ix));
    outMsg(logFile, ", exit=%d", entry->seg.segNo);
  }

  if (seg->seg.args != Null || seg->seg.locals != Null) {
    outMsg(logFile, "\n");

    if (seg->seg.args != Null)
      for (i = 0; i < seg->seg.arity; i++)
        showVar("A", i, &seg->seg.args[i]);
    if (seg->seg.locals != NULL)
      for (i = 0; i < seg->seg.lclCount; i++)
        showVar("L", i + 1, &seg->seg.locals[i]);
  }

  outMsg(logFile, "\n%_");
}

void showGroup(vectorPo group, integer groupNo) {
  outMsg(logFile, "group %d\n", groupNo);
  for (integer sx = 0; sx < vectLength(group); sx++) {
    segPo seg = O_SEG(getVectEl(group, sx));
    showSeg(seg);
  }
}

void showGroups(vectorPo groups, char *name) {
  outMsg(logFile, "segment map for %s\n", name);
  for (integer gx = 0; gx < vectLength(groups); gx++) {
    vectorPo group = O_VECT(getVectEl(groups, gx));
    showGroup(group, gx);
  }
  flushFile(logFile);
}
