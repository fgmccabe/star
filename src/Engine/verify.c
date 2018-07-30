//
// Created by Francis McCabe on 7/26/18.
//

#include <stdlib.h>
#include <globals.h>
#include "verify.h"

logical enableVerify = True;         // True if we verify code as it is loaded

typedef struct {
  logical inited;    //  True if cell has real value
  logical read;      //  Has this cell been read?
} Var, *varPo;

typedef struct _segment_ *segPo;

typedef struct _segment_ {
  integer segNo;                    // Segment number
  varPo args;
  integer lclCount;                 //  number of locals in use
  varPo locals;                     //  entry state for this segment
  integer arity;                    //  Arity of the code segment
  integer hpCnt;                    //  how much local heap can we allocate?
  logical checked;                  //  Has this segment been checked?
  methodPo mtd;                     //  Pointer to the code structure itself
  integer pc;                       //  base intruction of this segment
  integer maxPc;                    //  Maximum instruction in this segment
  segPo next;                       //  next segment in list
  segPo alt;                        // Alternate continuation
  segPo exit;                       // Normal continuation
  integer entryPoints;              //  how many entry points are there here?
} segRecord;

static poolPo segPool = Null;

static segPo initVerify(methodPo mtd);
static varPo copyVars(varPo vars, integer count);
static void clearSegs(segPo seg);;
static segPo findSeg(segPo root, integer pc);
static segPo splitSeg(segPo root, integer pc, integer tgt, logical isAlt, logical isNormal);
static int noOfSegments(segPo root);
static retCode mergeSegVars(segPo seg, segPo next, char *errorMsg, long msgLen);
static retCode checkSegment(segPo root, segPo seg, char *errorMsg, long msgLen);
static retCode checkSegments(segPo root, char *errorMsg, long msgLen);
static void showSegs(segPo root, char *name);
static void showSeg(segPo seg);
static retCode
testBreak(segPo root, insPo base, integer oPc, integer *pc, OpCode op, opAndSpec A, char *errorMsg, long msgLen);
static int32 collect32(insPo base, integer *pc);
static retCode verifyCode(methodPo mtd, segPo root, char *name, char *errorMsg, long msgLen);

#undef instruction
#define instruction(Op, A1, Delta, Cmt)\
    case Op:\
      ret=testBreak(root,base,oPc,&pc,Op,A1,errorMsg,msgLen);\
      continue;

retCode verifyMethod(methodPo mtd, char *name, char *errorMsg, long msgLen) {
  static pthread_mutex_t verifyMutex = PTHREAD_MUTEX_INITIALIZER;

  pthread_mutex_lock(&verifyMutex);  //  We synchronize all verification
  segPo root = initVerify(mtd);

  retCode ret = verifyCode(mtd, root, name, errorMsg, msgLen);
  clearSegs(root);
  pthread_mutex_unlock(&verifyMutex);  //  We can now allow others to verify

  return ret;
}

retCode verifyCode(methodPo mtd, segPo root, char *name, char *errorMsg, long msgLen) {
  integer codeLength = insCount(mtd);
  insPo base = entryPoint(mtd);
  integer pc = 0;

  retCode ret = Ok;

  while (ret == Ok && pc < codeLength) {
    integer oPc = pc;
    switch (base[pc++]) {
#include "instructions.h"

      default:
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid instruction at %d" RED_ESC_OFF, pc);
        return Error;
    }
  }

#ifdef TRACEVERIFY
  if (traceVerify)
    showSegs(root, name);
#endif

  if (ret == Ok)
    ret = checkSegments(root, errorMsg, msgLen);

  return ret;
}

retCode
testBreak(segPo root, insPo base, integer oPc, integer *pc, OpCode op, opAndSpec A, char *errorMsg, long msgLen) {
  retCode ret = Ok;

  switch (A) {
    case nOp:                                   // No operand
    case tOs:
      break;
    case i32:          /* 32 bit literal operand */
    case arg:          /* argument variable offset */
    case lcl:          /* local variable offset */
    case lcs: {        // Store to local variable
      collect32(base, pc);
      break;
    }
    case off: {         /* offset within current code */
      integer delta = collect32(base, pc);
      integer nPc = *pc + delta;
      segPo alt = splitSeg(root, oPc, nPc, True, True);
      if (alt == Null || splitSeg(root, oPc, *pc, False, (logical)(op!=Jmp)) == Null) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid destination address %d @ %d" RED_ESC_OFF, delta, oPc);
        return Error;
      }
      break;
    }
    case Es:
    case lit:          /* constant literal */
    case glb:           // Global variable name
      collect32(base, pc);
      return Ok;
    default:
      strMsg(errorMsg, msgLen, RED_ESC_ON "invalid operand specifier %d @ %d" RED_ESC_OFF, A, pc);
      return Error;
  }

  switch (op) {
    case Halt:
    case Tail:
    case OTail:
    case Ret:
    case Jmp: {
      splitSeg(root, oPc, *pc, False, False);
      break;
    }

    default:;
  }

  return ret;
}

segPo initVerify(methodPo mtd) {
  if (segPool == Null) {
    segPool = newPool(sizeof(segRecord), 16);
  }

  segPo first = (segPo) allocPool(segPool);

  first->segNo = 0;
  first->mtd = mtd;
  first->arity = mtd->arity;
  first->pc = 0;
  first->maxPc = insCount(mtd);
  first->next = first->alt = first->exit = Null;
  first->entryPoints = 1;
  first->checked = False;
  first->hpCnt = 0;
  first->lclCount = mtd->lclcnt;

  first->args = (varPo) malloc(sizeof(Var) * mtd->arity);
  first->locals = (varPo) malloc(sizeof(Var) * mtd->lclcnt);

  for (integer ix = 0; ix < mtd->arity; ix++) {
    varPo a = &first->args[ix];
    a->inited = True;
    a->read = False;
  }

  for (integer ix = 0; ix < mtd->lclcnt; ix++) {
    varPo l = &first->locals[ix];
    l->inited = False;
    l->read = False;
  }
  return first;
}

varPo copyVars(varPo vars, integer count) {
  varPo nv = (varPo) malloc(sizeof(Var) * count);

  for (integer ix = 0; ix < count; ix++)
    nv[ix] = vars[ix];

  return nv;
}

void clearSegs(segPo seg) {
  while (seg != Null) {
    segPo next = seg->next;

    if (seg->locals != Null) {
      free(seg->locals);
      seg->locals = Null;
    }
    if (seg->args != Null) {
      free(seg->args);
      seg->args = Null;
    }

    freePool(segPool, seg);
    seg = next;
  }
}

segPo findSeg(segPo root, integer pc) {
  while (root != NULL && pc >= root->maxPc)
    root = root->next;

  if (root == NULL || pc < root->pc || pc >= root->maxPc)
    return NULL;
  return root;
}

segPo splitSeg(segPo root, integer pc, integer tgt, logical isAlt, logical isNormal) {
  segPo base = findSeg(root, pc);
  segPo seg = findSeg(root, tgt);

  if (seg != Null) {
    if (tgt == seg->pc) {
      if (isNormal) {
        seg->entryPoints++;
      } else
        base->exit = Null;

      if (isAlt) {
        assert(base->alt == NULL);
        base->alt = seg;
      }

      return seg;                  // Nothing to do here
    } else {
      segPo new = (segPo) allocPool(segPool);

      assert(base->pc <= pc && base->maxPc > pc);

      if (isAlt) {
        assert(base->alt == NULL);
        base->alt = new;
      }
      new->exit = seg->exit;

      if (isNormal) {
        seg->exit = new;
      } else {
//        if (seg->exit != Null)
//          seg->exit->entryPoints--;
        seg->exit = Null;
      }

      new->args = copyVars(seg->args, seg->arity);

      for (integer i = 0; i < seg->arity; i++) {
        new->args[i].read = False;
      }

      new->mtd = seg->mtd;
      new->maxPc = seg->maxPc;
      seg->maxPc = tgt;
      new->segNo = noOfSegments(root);
      new->entryPoints = isNormal ? 1 : 0;
      new->arity = seg->arity;
      new->lclCount = seg->lclCount;
      new->checked = False;
      new->locals = copyVars(seg->locals, seg->lclCount);
      new->alt = NULL;
      new->pc = tgt;
      new->next = seg->next;
      seg->next = new;

      return new;
    }
  } else
    return Null;
}

retCode mergeSegVars(segPo seg, segPo next, char *errorMsg, long msgLen) {
  if (next->locals == NULL && seg->locals != NULL) {
    next->locals = copyVars(seg->locals, seg->lclCount);
  } else if (next->lclCount > seg->lclCount) {
    strMsg(errorMsg, msgLen, RED_ESC_ON "improper reallocation of locals" RED_ESC_OFF);
    return Error;
  } else if (seg->locals != NULL) {
    for (integer i = 0; i < next->lclCount; i++) {
      next->locals[i].inited &= seg->locals[i].inited;
      next->locals[i].read |= seg->locals[i].read;
    }
  }
  for (integer i = 0; i < seg->arity; i++) {
    next->args[i].inited &= seg->args[i].inited;
    next->args[i].read |= seg->args[i].read;
  }

  return Ok;
}

int32 collect32(insPo base, integer *pc) {
  uint32 hi = (uint32) base[(*pc)++];
  uint32 lo = (uint32) base[(*pc)++];
  return (int32) (hi << (uint32) 16 | lo);
}

static retCode
checkInstruction(segPo root, segPo seg, OpCode op, integer *pc, opAndSpec A1, char *errorMsg, long msgLen);

static retCode checkOperand(segPo root, segPo seg, integer *pc, opAndSpec A, char *errorMsg, long msgLen) {
  insPo base = entryPoint(seg->mtd);

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
      int32 argNo = collect32(base, pc) - 1;
      if (argNo >= 0 && argNo < seg->arity) {
        if (seg->args[argNo].inited) {
          seg->args[argNo].read = True;
          return Ok;
        } else {
          strMsg(errorMsg, msgLen, RED_ESC_ON "access to uninitialized argument %d @ %d" RED_ESC_OFF, argNo + 1, pc);
          return Error;
        }
      } else {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid argument number %d @ %d" RED_ESC_OFF, argNo, pc);
        return Error;
      }
    }
    case lcl: {                       /* local variable access */
      int32 lclVr = collect32(base, pc) - 1;
      if (lclVr >= 0 && lclVr < seg->lclCount) {
        if (seg->locals[lclVr].inited) {
          seg->locals[lclVr].read = True;
          return Ok;
        } else {
          strMsg(errorMsg, msgLen, RED_ESC_ON "access to uninitialized local var %d @ %d" RED_ESC_OFF, lclVr + 1, pc);
          return Error;
        }
      } else {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid local var number %d @ %d" RED_ESC_OFF, lclVr + 1, pc);
        return Error;
      }
    }
    case lcs: {                        // Store to local variable
      int32 lclVr = collect32(base, pc) - 1;
      if (lclVr >= 0 && lclVr < seg->lclCount) {
        seg->locals[lclVr].inited = True;
        seg->locals[lclVr].read = False;
        return Ok;
      } else {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid local var number %d @ %d" RED_ESC_OFF, lclVr + 1, pc);
        return Error;
      }
    }
    case off: {                         /* offset within current code */
      int32 delta = collect32(base, pc);
      integer npc = *pc + delta;
      segPo alt = findSeg(root, npc);

      if (alt == Null || alt->pc != npc) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid target of branch: %d @ %d" RED_ESC_OFF, npc, pc);
        return Error;
      } else {
        return mergeSegVars(seg, alt, errorMsg, msgLen);
      }
    }
    case Es: {                          // escape code
      int32 escNo = collect32(base, pc);

      if (getEscape(escNo) == Null) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid escape number: %d @ %d" RED_ESC_OFF, escNo, pc);
        return Error;
      }
      return Ok;
    }
    case lit: {                          /* constant literal */
      int32 litNo = collect32(base, pc);
      if (litNo < 0 || litNo >= codeLitCount(seg->mtd)) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid literal number: %d @ %d" RED_ESC_OFF, litNo, pc);
        return Error;
      }

      return Ok;
    }
    case glb: {                          // Global variable name
      int32 glbNo = collect32(base, pc);

      if (!isValidGlobalVarNo(glbNo)) {
        strMsg(errorMsg, msgLen, RED_ESC_ON "invalid global variable number: %d @ %d" RED_ESC_OFF, glbNo, pc);
        return Error;
      }
      return Ok;
    }
    default:
      strMsg(errorMsg, msgLen, RED_ESC_ON "invalid instruction @ %d" RED_ESC_OFF, pc);
      return Error;
  }
}

retCode checkInstruction(segPo root, segPo seg, OpCode op, integer *pc, opAndSpec A1, char *errorMsg, long msgLen) {
  retCode ret = checkOperand(root, seg, pc, A1, errorMsg, msgLen);

  if (ret == Ok) {
    // Special handling for specific instructions
    switch (op) {
      default:;
    }
  }

  return ret;
}

#undef instruction
#define instruction(Mn, A1, Dlta, Cmt)\
  case Mn:\
    ret = checkInstruction(root,seg,Mn,&pc,A1,errorMsg,msgLen);\
    continue;

retCode checkSegment(segPo root, segPo seg, char *errorMsg, long msgLen) {
  if (!seg->checked) {
    integer pc = seg->pc;
    integer limit = seg->maxPc;
    retCode ret = Ok;

    if (traceVerify) {
      outMsg(logFile, "On entry: ");
      showSeg(seg);
    }

    seg->checked = True;
    insPo base = entryPoint(seg->mtd);

    while (ret == Ok && pc < limit) {
      switch (base[pc++]) {
#include "instructions.h"

        default:
          strMsg(errorMsg, msgLen, RED_ESC_ON "illegal instruction at %d" RED_ESC_OFF, pc);
          return Error;
      }
    }
    if (traceVerify) {
      outMsg(logFile, "On exit:  ");
      showSeg(seg);
    }
  }
  return Ok;
}

static retCode checkSegments(segPo root, char *errorMsg, long msgLen) {
  int count = noOfSegments(root);
  segPo stack[count];
  int top = 0;

  stack[top++] = root;
  retCode ret = Ok;

  while (ret == Ok && top > 0) {
    segPo seg = stack[--top];
    ret = checkSegment(root, seg, errorMsg, msgLen);

    if (ret != Ok)
      return ret;

    if (seg->next != NULL) {
      segPo next = seg->next;

      if (!next->checked) {
        if ((ret = mergeSegVars(seg, next, errorMsg, msgLen)) != Ok)
          return ret;

        if (--next->entryPoints == 0)
          stack[top++] = next;
      }
    }
    if (seg->alt != NULL) {
      segPo alt = seg->alt;

      if (!alt->checked) {
        if ((ret = mergeSegVars(seg, alt, errorMsg, msgLen)) != Ok)
          return ret;

        if (--alt->entryPoints == 0)
          stack[top++] = alt;
      }
    }
  }

  for (segPo seg = root; seg != NULL; seg = seg->next)
    if (!seg->checked) {
      strMsg(errorMsg, msgLen, RED_ESC_ON "unreachable segment %d @ %x" RED_ESC_OFF, seg->segNo, seg->pc);
      return Error;
    }

  return Ok;
}

int noOfSegments(segPo root) {
  int i;

  for (i = 0; root != NULL; i++, root = root->next);
  return i;
}

void showSegs(segPo root, char *name) {
  segPo seg = root;

  outMsg(logFile, "%d segments for %s\n", noOfSegments(root), name);

  while (seg != NULL) {
    showSeg(seg);

    seg = seg->next;
  }
}

static void showVar(char *nm, integer ix, varPo v);

void showSeg(segPo seg) {
  integer i;

  outMsg(logFile, "segment: %d (%d) [%d-%d](%d)",
         seg->segNo, seg->entryPoints, seg->pc, seg->maxPc, seg->maxPc - seg->pc);

  if (seg->alt != NULL)
    outMsg(logFile, ", alt=%d", seg->alt->segNo);
  if (seg->exit != Null)
    outMsg(logFile, ", exit=%d", seg->exit->segNo);

  if (seg->arity > 0 || seg->lclCount > 0) {
    outMsg(logFile, "\n");

    if (seg->args != Null)
      for (i = 0; i < seg->arity; i++)
        showVar("A", i, &seg->args[i]);
    if (seg->locals != NULL)
      for (i = 0; i < seg->lclCount; i++)
        showVar("L", i, &seg->locals[i]);
  }

  outMsg(logFile, "\n");
  flushFile(logFile);
}

void showVar(char *nm, integer ix, varPo v) {
  outMsg(logFile, " %s[%d]%s%s", nm, ix, v->inited ? "*" : "", v->read ? "R" : "");
}
