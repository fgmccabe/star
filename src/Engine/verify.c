//
// Created by Francis McCabe on 7/26/18.
//

#include <stdlib.h>
#include <globals.h>
#include "verifyP.h"

logical enableVerify = True;         // True if we verify code as it is loaded

typedef struct {
  segPo *stack;
  int max;
  int top;
} BlockStack, *blockStackPo;

static segPo initVerify(methodPo mtd, vectorPo blocks);

static segPo findSeg(vectorPo blocks, integer pc);
static segPo splitSeg(vectorPo blocks, integer tgt);
static varPo copyVars(varPo src, integer count);
static retCode mergeSegVars(segPo seg, segPo next, char *errorMsg, long msgLen);
static retCode checkSegment(vectorPo blocks, blockStackPo stack, segPo seg, char *errorMsg, long msgLen);
static void showSegs(vectorPo blocks, char *name);
static retCode showSeg(ioPo out, segPo seg);
static int32 collect32(insPo base, integer *pc);
static void pushBlock(blockStackPo stack, segPo seg);
static segPo popBlock(blockStackPo stack);
static logical stackEmpty(blockStackPo stack);

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
  s->seg.hpCnt = 0;
  s->seg.lclCount = lclCount(s->seg.mtd);

  logical initVars = va_arg(*args, logical);

  if (initVars) {
    s->seg.args = (varPo) malloc(sizeof(Var) * s->seg.arity);
    s->seg.locals = (varPo) malloc(sizeof(Var) * s->seg.lclCount);
    s->seg.numExits = 0;

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

segPo newSegment(int segNo, methodPo mtd, integer pc, integer maxPc, logical initVars) {
  return (segPo) newObject(segmentClass, segNo, mtd, pc, maxPc, initVars);
}

void segmentDestroy(objectPo o) {
  segPo v = O_SEG(o);

  if (v->seg.args != Null)
    free(v->seg.args);
  v->seg.args = Null;

  if (v->seg.locals != Null)
    free(v->seg.locals);
  v->seg.locals = Null;
}

static integer segmentHash(objectPo o) {
  segPo s = O_SEG(o);
  return s->seg.segNo;
}

static logical segmentEquality(objectPo o1, objectPo o2) {
  segPo s1 = O_SEG(o1);
  segPo s2 = O_SEG(o2);
  return s1->seg.segNo == s2->seg.segNo;
}

static retCode
splitIns(vectorPo blocks, insPo code, integer *pc, integer to, logical jmpSplit, char *errorMsg, long msgLen);

retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen) {
  vectorPo blocks = vector(0);
  initVerify(mtd, blocks);

  integer pc = 0;
  retCode ret = splitIns(blocks, entryPoint(mtd), &pc, insCount(mtd), True, errorMsg, msgLen);

#ifdef TRACEVERIFY
  if (traceVerify)
    showSegs(blocks, name);
#endif

  if (ret == Ok) {
    integer count = vectLength(blocks);
    segPo stk[count];
    BlockStack stack = {.top=0, .max=(int) count, .stack=stk};

    pushBlock(&stack, O_SEG(getVectEl(blocks, 0)));

    while (ret == Ok && !stackEmpty(&stack)) {
      segPo seg = popBlock(&stack);
      ret = checkSegment(blocks, &stack, seg, errorMsg, msgLen);
    }

    if (ret == Ok) {
      for (integer ix = 0; ix < vectLength(blocks); ix++) {
        segPo seg = O_SEG(getVectEl(blocks, ix));
        if (!seg->seg.checked) {
          strMsg(errorMsg, msgLen, RED_ESC_ON "unreachable segment %d @ %x" RED_ESC_OFF, seg->seg.segNo, seg->seg.pc);
          ret = Error;
          break;
        }
      }
    }
  }

#ifdef TRACEVERIFY
  if (traceVerify)
    showSegs(blocks, name);
#endif

  decReference(O_OBJECT(blocks));

  return ret;
}

segPo initVerify(methodPo mtd, vectorPo blocks) {
  segPo first = newSegment(0, mtd, 0, insCount(mtd), True);

  appendVectEl(blocks, O_OBJECT(first));
  return first;
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

retCode splitIns(vectorPo blocks, insPo code, integer *pc, integer to, logical jmpSplit, char *errorMsg, long msgLen) {
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
    case arg:          /* argument variable offset */
    case lcl:          /* local variable offset */
    case lcs: {        // Store to local variable
      collect32(code, pc);
      break;
    }
    case off: {         /* offset within current code */
      integer delta = collect32(code, pc);
      integer nPc = *pc + delta;
      segPo tgt = splitSeg(blocks, nPc);
      tgt->seg.entryPoints++;

      segPo src = findSeg(blocks, oPc);
      assert(src->seg.numExits < NumberOf(src->seg.exits));
      src->seg.exits[src->seg.numExits++] = tgt->seg.segNo;

      break;
    }
    case Es:
    case lit:          /* constant literal */
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
      case Abort:
      case Ret: {
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
    segPo new = newSegment((int) vectLength(blocks), seg->seg.mtd, tgt, seg->seg.maxPc, False);

    seg->seg.maxPc = tgt;

    appendVectEl(blocks, O_OBJECT(new));

    return new;
  } else
    return seg;
}

// Phase 2: verify instructions are behaving

retCode mergeSegVars(segPo seg, segPo next, char *errorMsg, long msgLen) {
  if (next->seg.locals == Null) {
    next->seg.locals = copyVars(seg->seg.locals, seg->seg.lclCount);
    next->seg.args = copyVars(seg->seg.args, seg->seg.arity);
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
  }

  return Ok;
}

int32 collect32(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

static retCode
checkInstruction(vectorPo blocks, blockStackPo stack, segPo seg, OpCode op, integer *pc, opAndSpec A1, char *errorMsg,
                 long msgLen);

static retCode
checkOperand(vectorPo blocks, blockStackPo stack, segPo seg, integer *pc, opAndSpec A, char *errorMsg, long msgLen) {
  insPo base = entryPoint(seg->seg.mtd);

  switch (A) {
    case nOp:                                   // No operand
      return Ok;
    case tOs:
      return Ok;
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
          strMsg(errorMsg, msgLen, RED_ESC_ON " access to uninitialized local var %d @ %d" RED_ESC_OFF, lclVr + 1,
                 *pc);
          return Error;
        }
      } else {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid local var number %d @ %d" RED_ESC_OFF, lclVr + 1, *pc);
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
        return Error;
      }
    }
    case off: {                         /* offset within current code */
      int32 delta = collect32(base, pc);
      integer npc = *pc + delta;
      segPo alt = findSeg(blocks, npc);

      if (alt == Null || alt->seg.pc != npc) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid target of branch: %d @ %d" RED_ESC_OFF, npc, *pc);
        return Error;
      } else {
        if (--alt->seg.entryPoints <= 0)
          pushBlock(stack, alt);
        return mergeSegVars(seg, alt, errorMsg, msgLen);
      }
    }
    case Es: {                          // escape code
      int32 escNo = collect32(base, pc);

      if (getEscape(escNo) == Null) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid escape number: %d @ %d" RED_ESC_OFF, escNo, *pc);
        return Error;
      }
      return Ok;
    }
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
checkInstruction(vectorPo blocks, blockStackPo stack, segPo seg, OpCode op, integer *pc, opAndSpec A1, char *errorMsg,
                 long msgLen) {
  integer iPc = *pc;
  retCode ret = checkOperand(blocks, stack, seg, pc, A1, errorMsg, msgLen);

  if (ret == Ok) {
    // Special handling for specific instructions
    switch (op) {
      default:;
    }
  }

  return ret;
}

void pushBlock(blockStackPo stack, segPo seg) {
  assert(stack->top < stack->max);
  stack->stack[stack->top++] = seg;
}

segPo popBlock(blockStackPo stack) {
  assert(stack->top > 0);
  return stack->stack[--stack->top];
}

logical stackEmpty(blockStackPo stack) {
  return (logical) (stack->top == 0);
}

#undef instruction
#define instruction(Mn, A1, Dlta, Cmt)\
  case Mn:\
    ret = checkInstruction(blocks,stack,seg,Mn,&pc,A1,errorMsg,msgLen);\
    continue;

retCode checkSegment(vectorPo blocks, blockStackPo stack, segPo seg, char *errorMsg, long msgLen) {
  if (!seg->seg.checked) {
    integer pc = seg->seg.pc;
    integer limit = seg->seg.maxPc;
    retCode ret = Ok;

    if (traceVerify) {
      outMsg(logFile, "On entry: ");
      showSeg(logFile, seg);
    }

    seg->seg.checked = True;
    insPo base = entryPoint(seg->seg.mtd);

    while (ret == Ok && pc < limit) {
      switch (base[pc++]) {
#include "instructions.h"

        default:
          strMsg(errorMsg, msgLen, RED_ESC_ON "illegal instruction at %d" RED_ESC_OFF, pc);
          return Error;
      }
    }
    if (traceVerify) {
      if (ret != Ok)
        outMsg(logFile, "Error: block: %d, %s\n", seg->seg.segNo, errorMsg);
      outMsg(logFile, "On exit:  ");
      showSeg(logFile, seg);
    }
    return ret;
  }
  return Ok;
}

void showSegs(vectorPo blocks, char *name) {
  outMsg(logFile, "segment map for %s\n", name);
  for (integer ix = 0; ix < vectLength(blocks); ix++)
    showSeg(logFile, O_SEG(getVectEl(blocks, ix)));
}

static void showVar(char *nm, integer ix, varPo v);

retCode showSeg(ioPo out, segPo seg) {
  integer i;

  outMsg(out, "segment:%s %d (%d) [%d-%d](%d)",
         seg->seg.checked ? "¶" : "",
         seg->seg.segNo, seg->seg.entryPoints, seg->seg.pc, seg->seg.maxPc, seg->seg.maxPc - seg->seg.pc);

  for (int ix = 0; ix < seg->seg.numExits; ix++) {
    outMsg(out, ", exit=%d", seg->seg.exits[ix]);
  }

  if (seg->seg.args != Null || seg->seg.locals != Null) {
    outMsg(out, "\n");

    if (seg->seg.args != Null)
      for (i = 0; i < seg->seg.arity; i++)
        showVar("A", i, &seg->seg.args[i]);
    if (seg->seg.locals != NULL)
      for (i = 0; i < seg->seg.lclCount; i++)
        showVar("L", i + 1, &seg->seg.locals[i]);
  }

  outMsg(out, "\n");
  flushFile(out);
  return Ok;
}

void showVar(char *nm, integer ix, varPo v) {
  outMsg(logFile, " %s[%d]%s%s", nm, ix, v->inited ? "*" : "", v->read ? "R" : "");
}
