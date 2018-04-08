// Incremental instruction debugger

#include "engineP.h"
#include <stdlib.h>
#include <globals.h>
#include <str.h>

#include "debug.h"
#include "arith.h"

typedef struct _break_point_ {
  char pkgNm[MAX_SYMB_LEN];
  integer lineNo;
  integer offset;
} BreakPoint, *breakPointPo;

static retCode addBreakPoint(breakPointPo bp);
static logical breakPointHit(normalPo loc);
static retCode clearBreakPoint(breakPointPo bp);
static retCode parseBreakPoint(char *buffer, long bLen, breakPointPo bp);
static logical sameBreakPoint(breakPointPo b1, breakPointPo b2);
static logical breakPointInUse(breakPointPo b);
static void markBpOutOfUse(breakPointPo b);

static void showCall(ioPo out, methodPo mtd, termPo call);
static void showLine(ioPo out, methodPo mtd, termPo ln);
static void showRet(ioPo out, methodPo mtd, termPo call);

static void showRegisters(processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp);
static void showAllLocals(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp);
static retCode showLcl(ioPo out, integer vr, methodPo mtd, framePo fp, ptrPo sp);
static retCode showArg(ioPo out, integer arg, methodPo mtd, framePo fp, ptrPo sp);
static void showAllArgs(ioPo out, processPo p, methodPo mtd, framePo fp, ptrPo sp);
static void showAllStack(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp);
static void showStack(ioPo out, processPo p, methodPo mtd, integer vr, framePo fp, ptrPo sp);
static retCode localVName(methodPo mtd, insPo pc, integer vNo, char *buffer, integer bufLen);

static insPo disass(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp);

static inline int32 collect32(insPo *pc) {
  uint32 hi = (uint32) (*(*pc)++);
  uint32 lo = (uint32) (*(*pc)++);
  return (int32) (hi << 16 | lo);
}

#define collectI32(pc) (collI32(pc))
#define collI32(pc) hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<16)|lo32)

retCode g__ins_debug(processPo P, ptrPo a) {
  insDebugging = tracing = True;
  return Ok;
}

static integer cmdCount(char *cmdLine) {
  while (isSpaceChar((codePoint) *cmdLine))
    cmdLine++;
  return parseInteger(cmdLine, uniStrLen((char *) cmdLine));
}

static processPo focus = NULL;
static pthread_mutex_t debugMutex = PTHREAD_MUTEX_INITIALIZER;

static BreakPoint breakPoints[10];
static int breakPointCount = 0;

retCode addBreakPoint(breakPointPo bp) {
  for (int ix = 0; ix < breakPointCount; ix++) {
    if (!breakPointInUse(&breakPoints[ix])) {
      breakPoints[ix] = *bp;
      return Ok;
    }
  }
  if (breakPointCount < NumberOf(breakPoints)) {
    breakPoints[breakPointCount++] = *bp;
    return Ok;
  } else
    return Fail;
}

logical sameBreakPoint(breakPointPo b1, breakPointPo b2) {
  return (logical) (uniCmp(b1->pkgNm, b2->pkgNm) == same && b1->lineNo == b2->lineNo && b1->offset == b2->offset);
}

logical isBreakPoint(breakPointPo b, const char *pk, integer lineNo, integer offset) {
  if (uniCmp(b->pkgNm, pk) == same && b->lineNo == lineNo) {
    if (offset != -1)
      return (logical) (b->offset == -1 || b->offset == offset);
    else
      return True;
  }
  return False;
}

logical breakPointHit(normalPo loc) {
  char pkgNm[MAX_SYMB_LEN];

  copyString2Buff(C_STR(nthArg(loc, 0)), pkgNm, NumberOf(pkgNm));
  integer lineNo = integerVal(nthArg(loc, 1));
  integer offset = integerVal(nthArg(loc, 2));

  for (int ix = 0; ix < breakPointCount; ix++) {
    if (isBreakPoint(&breakPoints[ix], pkgNm, lineNo, offset))
      return True;
  }
  return False;
}

logical breakPointInUse(breakPointPo b) {
  return (logical) (b->lineNo >= 0);
}

void markBpOutOfUse(breakPointPo b) {
  b->lineNo = -1;
}

retCode clearBreakPoint(breakPointPo bp) {
  for (int ix = 0; ix < breakPointCount; ix++) {
    if (sameBreakPoint(bp, &breakPoints[ix])) {
      if (ix == breakPointCount - 1) {
        breakPointCount--;
        while (!breakPointInUse(&breakPoints[ix]))
          breakPointCount--;
        return Ok;
      } else {
        markBpOutOfUse(&breakPoints[ix]);
        return Ok;
      }
    }
  }

  return Fail;
}

/*
 * A Break point is specified as:
 * pkg/line
 * or
 * pkg/line:off
 */

static retCode parseBreakPoint(char *buffer, long bLen, breakPointPo bp) {
  integer b = 0;
  integer ix = 0;

  integer line = -1;
  integer offset = -1;

  enum {
    initSte,
    inPkg,
    inLine,
    inOffset
  } pState = initSte;

  while (ix < bLen && buffer[ix] == ' ')
    ix++;

  while (ix < bLen) {
    codePoint cp = nextCodePoint(buffer, &ix, bLen);
    switch (cp) {
      case '\n':
      case 0:
        appendCodePoint(bp->pkgNm, &b, NumberOf(bp->pkgNm), 0);
        bp->lineNo = line;
        bp->offset = offset;
        return Ok;
      case '/': {
        switch (pState) {
          case inPkg:
            appendCodePoint(bp->pkgNm, &b, NumberOf(bp->pkgNm), 0);
            pState = inLine;
            line = 0;
            continue;
          default:
            outMsg(logFile, "invalid break point: %S\n", buffer, bLen);
            return Error;
        }
      }
      case ':': {
        switch (pState) {
          case inLine:
            pState = inOffset;
            offset = 0;
            continue;
          default:
            outMsg(logFile, "invalid break point: %S\n", buffer, bLen);
            return Error;
        }
      }
      case ' ':
        switch (pState) {
          case initSte:
            continue;
          case inPkg:
            appendCodePoint(bp->pkgNm, &b, NumberOf(bp->pkgNm), 0);
            pState = inLine;
            line = offset = 0;
            continue;
          case inLine:
            pState = inOffset;
            continue;
          case inOffset:
            continue;
        }
      default:
        switch (pState) {
          case inPkg:
            appendCodePoint(bp->pkgNm, &b, NumberOf(bp->pkgNm), cp);
            continue;
          case inLine:
            if (isNdChar(cp)) {
              line = line * 10 + digitValue(cp);
              continue;
            } else {
              outMsg(logFile, "invalid break point line number: %S\n", buffer, bLen);
              return Error;
            }
          case inOffset:
            if (isNdChar(cp)) {
              offset = offset * 10 + digitValue(cp);
              continue;
            } else {
              outMsg(logFile, "invalid break point line offset: %S\n", buffer, bLen);
              return Error;
            }
          case initSte:
            if (!isSpaceChar(cp)) {
              pState = inPkg;
              appendCodePoint(bp->pkgNm, &b, NumberOf(bp->pkgNm), cp);
            }
            continue;
        }
    }
  }
  return Error;
}

void dC(termPo w) {
  outMsg(logFile, "%T\n", w);
  flushOut();
}

static retCode showConstant(ioPo out, methodPo mtd, integer off) {
  return outMsg(out, " %T", nthArg(mtd->pool, off));
}

static logical shouldWeStop(processPo p, methodPo mtd, insPo pc) {
  if (focus == NULL || focus == p) {
    switch (p->waitFor) {
      case nextIns:
        return True;
      case nextSucc:
        return (logical) (*pc == Ret);
      case nextBreak:
        if (*pc == Line) {
          int32 litNo = (uint32) (pc[1] << 16 | pc[2]);
          normalPo ln = C_TERM(getMtdLit(mtd, litNo));
          if (breakPointHit(ln)) {
            p->waitFor = nextIns;
            return True;
          }
        }
        return False;

      case never:
      default:
        return False;
    }
  } else
    return False;
}

typedef DebugWaitFor (*debugCmd)(char *line, processPo p, void *cl);

typedef struct {
  codePoint c;
  char *usage;
  void *cl;
  debugCmd cmd;
} DebugOption, *debugOptPo;

static DebugWaitFor
cmder(debugOptPo opts, int optCount, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  static char cmdLine[256];

  while (interactive) {
    outMsg(logFile, " => ");
    flushFile(logFile);
    integer cmdLen = 0;

    retCode res = inLine(stdIn, cmdLine, NumberOf(cmdLine), &cmdLen, "\n");

    if (res == Ok) {
      for (int ix = 0; ix < optCount; ix++) {
        if (opts[ix].c == cmdLine[0])
          return opts[ix].cmd(&cmdLine[1], p, opts[ix].cl);
      }
      outMsg(stdErr, "invalid debugger command: %s", cmdLine);
      for (int ix = 0; ix < optCount; ix++)
        outMsg(stdErr, "%s\n", opts[ix].usage);
      flushFile(stdErr);
      return moreDebug;
    }
  }
  return moreDebug;
}

static DebugWaitFor dbgSingle(char *line, processPo p, void *cl) {
  integer *traceCount = (integer *) cl;
  *traceCount = cmdCount(line);
  return nextIns;
}

static DebugWaitFor dbgQuit(char *line, processPo p, void *cl) {
  return quitDbg;
}

static DebugWaitFor dbgTrace(char *line, processPo p, void *cl) {
  p->tracing = True;
  return nextBreak;
}

static DebugWaitFor dbgCont(char *line, processPo p, void *cl) {
  p->tracing = False;
  return nextBreak;
}

static DebugWaitFor dbgRestart(char *line,processPo p,void *cl){
  p->pc = entryPoint(p->prog);
  p->sp = (ptrPo)p->fp;
  p->fp = (framePo)(*p->sp++);
  p->hasEnter = False;

  return moreDebug;
}

static DebugWaitFor dbgShowRegisters(char *line, processPo p, void *cl) {
  showRegisters(p, p->heap, p->prog, p->pc, p->fp, p->sp);
  return moreDebug;
}

static DebugWaitFor dbgShowArg(char *line, processPo p, void *cl) {
  integer argNo = cmdCount(line);
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;

  if (argNo == 0)
    showAllArgs(stdErr, p, mtd, fp, sp);
  else if (argNo > 0 && argNo < argCount(mtd))
    showArg(stdErr, argNo, mtd, fp, sp);
  else
    outMsg(stdErr, "invalid argument number: %d", argNo);
  return moreDebug;
}

static DebugWaitFor dbgShowLocal(char *line, processPo p, void *cl) {
  integer lclNo = cmdCount(line);
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;

  if (lclNo == 0)
    showAllLocals(stdErr, p, mtd, p->pc, fp);
  else if (lclNo > 0 && lclNo <= lclCount(mtd))
    showLcl(stdErr, cmdCount(line), mtd, fp, sp);
  else
    outMsg(stdErr, "invalid local number: %d", lclNo);
  return moreDebug;
}

static DebugWaitFor dbgShowStack(char *line, processPo p, void *cl) {
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;
  insPo pc = p->pc;

  if (line[0] == '\n') {
    showAllStack(stdErr, p, mtd, pc, fp, sp);
  } else
    showStack(stdErr, p, mtd, cmdCount(line), fp, sp);

  return moreDebug;
}

static DebugWaitFor dbgShowCode(char *line, processPo p, void *cl) {
  integer count = maximum(1, cmdCount(line));
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;
  insPo pc = p->pc;

  insPo last = entryPoint(mtd) + insCount(mtd);

  for (integer ix = 0; ix < count && pc < last; ix++) {
    pc = disass(stdErr, p, mtd, pc, Null, Null);
    outStr(stdErr, "\n");
  }

  flushFile(stdErr);

  return moreDebug;
}

static DebugWaitFor dbgInsDebug(char *line, processPo p, void *cl) {
  lineDebugging = False;
  insDebugging = True;
  return nextIns;
}

static DebugWaitFor dbgSymbolDebug(char *line, processPo p, void *cl) {
  lineDebugging = True;
  insDebugging = False;
  return nextIns;
}

static DebugWaitFor dbgAddBreakPoint(char *line, processPo p, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok)
    ret = addBreakPoint(&bp);
  if (ret != Ok)
    outMsg(logFile, "Could not set spy point on %s\n%_", line);
  else
    outMsg(logFile, "spy point set on %s\n%_", line);
  return moreDebug;
}

static DebugWaitFor dbgClearBreakPoint(char *line, processPo p, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok)
    ret = clearBreakPoint(&bp);
  if (ret != Ok)
    outMsg(logFile, "Could not clear spy point on %s\n%_", line);
  else
    outMsg(logFile, "spy point cleared on %s\n%_", line);
  return moreDebug;
}

DebugWaitFor insDebug(integer pcCount, processPo p) {
  static integer traceCount = 0;
  static DebugOption opts[] = {
    {.c = 'n', .cmd=dbgSingle, .usage="n single step", .cl=&traceCount},
    {.c = '\n', .cmd=dbgSingle, .usage="\\n single step", .cl=&traceCount},
    {.c = 'q', .cmd=dbgQuit, .usage="q stop execution", .cl=Null},
    {.c = 't', .cmd=dbgTrace, .usage="t trace mode"},
    {.c = 'c', .cmd=dbgCont, .usage="c continue"},
    {.c = 'R', .cmd=dbgRestart, .usage="R restart current function"},
    {.c = 'r', .cmd=dbgShowRegisters, .usage="r show registers"},
    {.c = 'l', .cmd=dbgShowLocal, .usage="l show local variable"},
    {.c = 'a', .cmd=dbgShowArg, .usage="a show argument"},
    {.c = 's', .cmd=dbgShowStack, .usage="s show stack"},
    {.c = 'i', .cmd=dbgShowCode, .usage="i show instructions"},
    {.c = '+', .cmd=dbgAddBreakPoint, .usage="+ add break point"},
    {.c = '-', .cmd=dbgClearBreakPoint, .usage="- clear break point"},
    {.c = 'y', .cmd=dbgSymbolDebug, .usage="y turn on symbolic mode"},
  };
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;
  insPo pc = p->pc;
  heapPo h = p->heap;

  logical stopping = shouldWeStop(p, mtd, pc);
  if (p->tracing || stopping) {
    outMsg(stdErr, "[%d]: ", pcCount);
    disass(stdErr, p, mtd, pc, fp, sp);
    if (stopping) {

      while (interactive) {
        if (traceCount == 0)
          p->waitFor = cmder(opts, NumberOf(opts), p, h, mtd, pc, fp, sp);
        else {
          traceCount--;
          outStr(stdErr, "\n");
          flushFile(stdErr);
        }

        switch (p->waitFor) {
          case moreDebug:
            continue;
          case nextIns:
          case nextBreak:
          case never:
          case nextSucc:
            return p->waitFor;
          case quitDbg:
            exit(0);
        }
      }
    } else {
      outStr(stdErr, "\n");
      flushFile(stdErr);
    }
  }
  return p->waitFor;
}

void showLine(ioPo out, methodPo mtd, termPo ln) {
  if (isNormalPo(ln)) {
    normalPo line = C_TERM(ln);
    integer pLen;
    const char *pkgNm = stringVal(nthArg(line, 0), &pLen);
    outMsg(out, "%S/%T:%T(%T) %T", pkgNm, pLen, nthArg(line, 1), nthArg(line, 2), nthArg(line, 4),
           nthArg(codeLits(mtd), 0));
    flushFile(out);
    return;
  } else
    outMsg(out, "line: %T", ln);
}

void showCall(ioPo out, methodPo mtd, termPo call) {
  outMsg(out, "call: %T", call);
}

void showTail(ioPo out, methodPo mtd, termPo call) {
  outMsg(out, "tail: %T", call);
}

void showRet(ioPo out, methodPo mtd, termPo call) {
  outMsg(out, "return: %T->%T", mtd, call);
}

typedef void (*showCmd)(ioPo out, methodPo mtd, termPo trm);

termPo insLit(processPo p) {
  insPo pc = p->pc + 1;
  int32 ix = collect32(&pc);
  return getMtdLit(p->prog, ix);
}

static DebugWaitFor lnDebug(processPo p, termPo ln, showCmd show);

DebugWaitFor callDebug(processPo p, termPo call) {
  return lnDebug(p, call, showCall);
}

DebugWaitFor tailDebug(processPo p, termPo call) {
  return lnDebug(p, call, showTail);
}

DebugWaitFor retDebug(processPo p, termPo call) {
  return lnDebug(p, call, showRet);
}

DebugWaitFor lineDebug(processPo p, termPo ln) {
  return lnDebug(p, ln, showLine);
}

DebugWaitFor lnDebug(processPo p, termPo ln, showCmd show) {
  static integer traceCount = 0;
  static DebugOption opts[] = {
    {.c = 'n', .cmd=dbgSingle, .usage="n single step", .cl=&traceCount},
    {.c = '\n', .cmd=dbgSingle, .usage="\\n single step", .cl=&traceCount},
    {.c = 'q', .cmd=dbgQuit, .usage="q stop execution"},
    {.c = 't', .cmd=dbgTrace, .usage="t trace mode"},
    {.c = 'c', .cmd=dbgCont, .usage="c continue"},
    {.c = 'R', .cmd=dbgRestart, .usage="R restart current function"},
    {.c = 'r', .cmd=dbgShowRegisters, .usage="r show registers"},
    {.c = 'a', .cmd=dbgShowArg, .usage="a show argument"},
    {.c = 's', .cmd=dbgShowStack, .usage="s show stack"},
    {.c = 'l', .cmd=dbgShowLocal, .usage="l show local variable"},
    {.c = 'i', .cmd=dbgShowCode, .usage="i show instructions"},
    {.c = '+', .cmd=dbgAddBreakPoint, .usage="+ add break point"},
    {.c = '-', .cmd=dbgClearBreakPoint, .usage="- clear break point"},
    {.c = 'y', .cmd=dbgInsDebug, .usage="y turn on instruction mode"},
  };
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;
  insPo pc = p->pc;
  heapPo h = p->heap;

  logical stopping = shouldWeStop(p, mtd, pc);
  if (p->tracing || stopping) {
    if (ln != Null)
      show(logFile, mtd, ln);
    if (stopping) {
      while (interactive) {
        if (traceCount == 0)
          p->waitFor = cmder(opts, NumberOf(opts), p, h, mtd, pc, fp, sp);
        else {
          traceCount--;
          outStr(stdErr, "\n");
          flushFile(stdErr);
        }

        switch (p->waitFor) {
          case moreDebug:
            continue;
          case nextIns:
          case nextBreak:
          case never:
          case nextSucc:
            return p->waitFor;
          case quitDbg:
            exit(0);
        }
      }
    } else {
      outStr(stdErr, "\n");
      flushFile(stdErr);
    }
  }
  return p->waitFor;
}

void showAllArgs(ioPo out, processPo p, methodPo mtd, framePo fp, ptrPo sp) {
  if (p->hasEnter) {
    integer count = argCount(mtd);
    for (integer ix = 0; ix < count; ix++) {
      outMsg(out, "A[%d] = %T\n", ix + 1, fp->args[ix]);
    }
  } else {
    outMsg(out, "cant show args\n");
    sp++;
  }
}

retCode showArg(ioPo out, integer arg, methodPo mtd, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null)
    return outMsg(out, " a[%d] = %T", arg, fp->args[arg - 1]);
  else
    return outMsg(out, " a[%d]", arg);
}

void showAllLocals(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp) {
  for (integer vx = 1; vx <= lclCount(mtd); vx++) {
    char vName[MAX_SYMB_LEN];
    if (localVName(mtd, pc, vx, vName, NumberOf(vName)) == Ok) {
      ptrPo var = localVar(fp, vx);
      outMsg(out, "%s[%d] = %T\n", vName, vx, *var);
    }
  }
}

retCode showLcl(ioPo out, integer vr, methodPo mtd, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null)
    return outMsg(out, " l[%d] = %T", vr, *localVar(fp, vr));
  else
    return outMsg(out, " l[%d]", vr);
}

void showAllStack(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  ptrPo stackTop = ((ptrPo) fp) - lclCount(mtd);

  if (!p->hasEnter)
    sp++;

  for (integer ix = 0; sp < stackTop; ix++, sp++) {
    outMsg(out, "SP[%d]=%T\n", ix, *sp);
  }
}

void showStack(ioPo out, processPo p, methodPo mtd, integer vr, framePo fp, ptrPo sp) {
  ptrPo stackTop = ((ptrPo) fp) - lclCount(mtd);

  if (!p->hasEnter)
    sp++;

  if (vr >= 0 && vr < stackTop - sp)
    outMsg(out, "SP[%d]=%T\n", vr, sp[vr]);
  else
    outMsg(out, "invalid stack offset: %d", vr);
}

insPo disass(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  int32 hi32, lo32;

  integer offset = (integer) (pc - entryPoint(mtd));

  outMsg(out, "0x%x: %T(%d) ", pc, nthArg(codeLits(mtd), 0), offset);

  switch (*pc++) {
#undef instruction

#define show_nOp
#define show_i32 outMsg(out," #%d",collectI32(pc))
#define show_arg showArg(out,collectI32(pc),mtd,fp,sp)
#define show_lcl showLcl(out,collectI32(pc),mtd,fp,sp)
#define show_lcs outMsg(out," l[%d]",collectI32(pc))
#define show_off outMsg(out," PC[%d]",collectI32(pc))
#define show_Es outMsg(out, " %s", getEscape(collectI32(pc))->name)
#define show_lit showConstant(out,mtd,collectI32(pc))
#define show_glb outMsg(out, " %s", globalVarName(getGlobalVar(collectI32(pc))))

#define instruction(Op, A1, Cmt)    \
    case Op:          \
      outMsg(out," %s",#Op);    \
      show_##A1;        \
  return pc;

#include "instructions.h"

    default:
      return pc;
  }
}

void showRegisters(processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  integer pcOffset = (integer) (pc - mtd->code);

  outMsg(logFile, "p: 0x%x, mtd: %T[%d], pc: 0x%x, fp: 0x%x, sp: 0x%x, ",
         p, mtd, pcOffset, pc, fp, sp);
  heapSummary(logFile, h);
  outMsg(logFile, "\n");

  ptrPo stackTop = ((ptrPo) fp) - mtd->lclcnt;

  showAllLocals(logFile, p, mtd, pc, fp);
  showAllArgs(logFile, p, mtd, fp, sp);

  if (!p->hasEnter)
    sp++;

  for (integer ix = 0; sp < stackTop; ix++, sp++) {
    termPo t = *sp;
    outMsg(logFile, "SP[%d]=%T\n", ix, t);
  }

  flushFile(logFile);
}

static char *anonPrefix = "__";

retCode localVName(methodPo mtd, insPo pc, integer vNo, char *buffer, integer bufLen) {
  normalPo locals = mtd->locals;
  int64 numLocals = termArity(locals);
  integer pcOffset = (integer) (pc - mtd->code);

  for (int32 ix = 0; ix < numLocals; ix++) {
    normalPo vr = C_TERM(nthArg(locals, ix));
    integer from = integerVal(nthArg(vr, 1));
    integer to = integerVal(nthArg(vr, 2));

    if (from <= pcOffset && to > pcOffset && integerVal(nthArg(vr, 3)) == vNo) {
      copyString2Buff(C_STR(nthArg(vr, 0)), buffer, bufLen);

      if (uniIsLitPrefix(buffer, anonPrefix))
        uniCpy(buffer, bufLen, "l");
      return Ok;
    }
  }
  return Fail;
}

static integer insCounts[illegalOp];

void countIns(insWord ins) {
  insCounts[ins]++;
}

#undef instruction
#define instruction(Op, Arg, Cmt) outMsg(logFile,#Op": %d\n",insCounts[Op]);

void dumpInsCount() {
#include "instructions.h"
}
