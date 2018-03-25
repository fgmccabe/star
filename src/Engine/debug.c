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

static inline int32 collect32(insPo *pc) {
  uint32 hi = (uint32) (*(*pc)++);
  uint32 lo = (uint32) (*(*pc)++);
  return (int32) (hi << 16 | lo);
}

#define collectI32(pc) (collI32(pc))
#define collI32(pc) hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<16)|lo32)

static retCode localVName(methodPo mtd, insPo pc, integer vNo, char *buffer, integer bufLen);

retCode g__ins_debug(processPo P, ptrPo a) {
  insDebugging = tracing = True;
  return Ok;
}

static integer cmdCount(char *cmdLine) {
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

static void showRegisters(heapPo h, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp);

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

typedef DebugWaitFor (*debugCmd)(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp,
                                 void *cl);

typedef struct {
  codePoint c;
  char *usage;
  void *cl;
  debugCmd cmd;
} DebugOption, *debugOptPo;

typedef retCode (*dissCmd)(processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl);

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
          return opts[ix].cmd(&cmdLine[1], p, h, mtd, pc, fp, sp, opts[ix].cl);
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

static DebugWaitFor
dbgSingle(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  integer *traceCount = (integer *) cl;
  *traceCount = cmdCount(line);
  return nextIns;
}

static DebugWaitFor dbgQuit(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  return quitDbg;
}

static DebugWaitFor
dbgTrace(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  p->tracing = True;
  return nextBreak;
}

static DebugWaitFor
dbgCont(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  p->tracing = False;
  return nextBreak;
}

static DebugWaitFor
dbgShowRegisters(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  showRegisters(h, p, mtd, pc, fp, sp);
  return moreDebug;
}

static DebugWaitFor
dbgShowLocal(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  showRegisters(processHeap(p), p, mtd, pc, fp, sp);
  return moreDebug;
}

static DebugWaitFor
dbgShowCode(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  showRegisters(processHeap(p), p, mtd, pc, fp, sp);
  return moreDebug;
}

static DebugWaitFor
dbgInsDebug(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  SymbolDebug = False;
  insDebugging = True;
  return nextIns;
}

static DebugWaitFor
dbgSymbolDebug(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
  SymbolDebug = True;
  insDebugging = False;
  return nextIns;
}

static DebugWaitFor
dbgAddBreakPoint(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
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

static DebugWaitFor
dbgClearBreakPoint(char *line, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp, void *cl) {
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

void insDebug(integer pcCount, processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  static integer traceCount = 0;

  logical stopping = shouldWeStop(p, mtd, pc);
  if (p->tracing || stopping) {
    disass(stdErr, pcCount, p, mtd, pc, fp, sp);
    if (stopping) {
      DebugOption opts[] = {
        {.c = 'n', .cmd=dbgSingle, .usage="n single step", .cl=&traceCount},
        {.c = '\n', .cmd=dbgSingle, .usage="\\n single step", .cl=&traceCount},
        {.c='q', .cmd=dbgQuit, .usage="q stop execution", .cl=Null},
        {.c='t', .cmd=dbgTrace, .usage="t trace mode"},
        {.c='c', .cmd=dbgCont, .usage="c continue"},
        {.c='r', .cmd=dbgShowRegisters, .usage="r show registers"},
        {.c='l', .cmd=dbgShowLocal, .usage="l show local variable"},
        {.c='i', .cmd=dbgShowCode, .usage="i show instructions"},
        {.c='+', .cmd=dbgAddBreakPoint, .usage="+ add break point"},
        {.c='-', .cmd=dbgClearBreakPoint, .usage="- clear break point"},
        {.c='S', .cmd=dbgSymbolDebug, .usage="S turn on symbolic mode"},
      };

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
            return;
          case quitDbg:
            exit(0);
        }
      }
    } else {
      outStr(stdErr, "\n");
      flushFile(stdErr);
    }
  }
}

static void showLine(ioPo out, termPo ln, methodPo mtd) {
  if (isNormalPo(ln)) {
    normalPo line = C_TERM(ln);
    integer pLen;
    const char *pkgNm = stringVal(nthArg(line, 0), &pLen);
    outMsg(out, "%S:%T(%T) %T", pkgNm, pLen, nthArg(line, 1), nthArg(line, 4), nthArg(codeLits(mtd), 0));
    flushFile(out);
    return;
  } else
    outMsg(out, "line: %T\n", ln);
}

void lineDebug(processPo p, heapPo h, methodPo mtd, termPo ln, insPo pc, framePo fp, ptrPo sp) {
  static integer traceCount = 0;

  logical stopping = shouldWeStop(p, mtd, pc);
  if (p->tracing || stopping) {
    showLine(logFile, ln, mtd);
    if (stopping) {
      DebugOption opts[] = {
        {.c = 'n', .cmd=dbgSingle, .usage="n single step", .cl=&traceCount},
        {.c = '\n', .cmd=dbgSingle, .usage="\\n single step", .cl=&traceCount},
        {.c='q', .cmd=dbgQuit, .usage="q stop execution", .cl=Null},
        {.c='t', .cmd=dbgTrace, .usage="t trace mode"},
        {.c='c', .cmd=dbgCont, .usage="c continue"},
        {.c='r', .cmd=dbgShowRegisters, .usage="r show registers"},
        {.c='l', .cmd=dbgShowLocal, .usage="l show local variable"},
        {.c='i', .cmd=dbgShowCode, .usage="i show instructions"},
        {.c='+', .cmd=dbgAddBreakPoint, .usage="+ add break point"},
        {.c='-', .cmd=dbgClearBreakPoint, .usage="- clear break point"},
        {.c='s', .cmd=dbgInsDebug, .usage="s turn on instruction mode"},
      };

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
            return;
          case quitDbg:
            exit(0);
        }
      }
    } else {
      outStr(stdErr, "\n");
      flushFile(stdErr);
    }
  }

}

static retCode showArg(ioPo out, integer arg, methodPo mtd, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null)
    return outMsg(out, " a[%d] = %T", arg, fp->args[arg - 1]);
  else
    return outMsg(out, " a[%d]", arg);
}

static retCode showLcl(ioPo out, integer vr, methodPo mtd, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null)
    return outMsg(out, " l[%d] = %T", vr, *localVar(fp, vr));
  else
    return outMsg(out, " l[%d]", vr);
}

insPo disass(ioPo out, integer pcCount, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  int32 hi32, lo32;

  integer offset = (integer) (pc - entryPoint(mtd));

  outMsg(out, "0x%x[%d]: %T(%d) ", pc, pcCount, nthArg(codeLits(mtd), 0), offset);

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

void showRegisters(heapPo h, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  integer pcOffset = (integer) (pc - mtd->code);

  outMsg(logFile, "p: 0x%x, mtd: %T[%d], pc: 0x%x, fp: 0x%x, sp: 0x%x, ",
         p, mtd, pcOffset, pc, fp, sp);
  heapSummary(logFile, h);
  outMsg(logFile, "\n");

  normalPo locals = mtd->locals;
  int64 numLocals = termArity(locals);

  ptrPo stackTop = ((ptrPo) fp) - mtd->lclcnt;

  for (integer vx = 0; vx < mtd->lclcnt; vx++) {
    char vName[MAX_SYMB_LEN];
    if (localVName(mtd, pc, vx, vName, NumberOf(vName)) == Ok) {
      ptrPo var = localVar(fp, vx);
      outMsg(logFile, "%s[%d] = %T\n", vName, vx, *var);
    }
  }

  if (p->hasEnter) {
    integer count = argCount(mtd);
    for (integer ix = 0; ix < count; ix++) {
      outMsg(logFile, "A[%d] = %T\n", ix + 1, fp->args[ix]);
    }
  } else {
    outMsg(logFile, "cant show args\n");
    sp++;
  }

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
