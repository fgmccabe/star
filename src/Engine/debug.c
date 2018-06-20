// Incremental instruction debugger

#include "engineP.h"
#include <stdlib.h>
#include <globals.h>
#include <str.h>

#include "debug.h"
#include "bkpoint.h"
#include "arith.h"

integer pcCount = 0;

static void showCall(ioPo out, methodPo mtd, termPo call, framePo fp, ptrPo sp);
static void showLn(ioPo out, methodPo mtd, termPo ln, framePo fp, ptrPo sp);
static void showRet(ioPo out, methodPo mtd, termPo val, framePo fp, ptrPo sp);

static void showRegisters(processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp);
static void showAllLocals(ioPo out, methodPo mtd, insPo pc, framePo fp);
static retCode showTos(ioPo out, framePo fp, ptrPo sp);
static retCode showLcl(ioPo out, integer vr, methodPo mtd, framePo fp, ptrPo sp);
static retCode showArg(ioPo out, integer arg, methodPo mtd, framePo fp, ptrPo sp);
static void showAllArgs(ioPo out, processPo p, methodPo mtd, framePo fp, ptrPo sp);
static void showAllStack(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp);
static void showStack(ioPo out, processPo p, methodPo mtd, integer vr, framePo fp, ptrPo sp);
static retCode localVName(methodPo mtd, insPo pc, integer vNo, char *buffer, integer bufLen);
static void stackSummary(ioPo out,processPo P,ptrPo sp);

static insPo disass(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp);

static inline int32 collect32(insPo pc) {
  uint32 hi = (uint32) pc[0];
  uint32 lo = (uint32) pc[1];
  return (int32) (hi << (uint32) 16 | lo);
}

#define collectI32(pc) (collI32(pc))
#define collI32(pc) hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<16)|lo32)

retCode g__ins_debug(processPo P, ptrPo a) {
  insDebugging = tracing = True;
  return Ok;
}

static integer cmdCount(char *cmdLine, integer deflt) {
  while (isSpaceChar((codePoint) *cmdLine))
    cmdLine++;
  if (uniStrLen(cmdLine) == 0)
    return deflt;
  else
    return parseInteger(cmdLine, uniStrLen((char *) cmdLine));
}

static processPo focus = NULL;
static pthread_mutex_t debugMutex = PTHREAD_MUTEX_INITIALIZER;

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
      case stepInto:
        if (p->traceCount > 0)
          p->traceCount--;
        return (logical) (p->traceCount == 0);
      case stepOver: {
        switch (*pc) {
          case dRet: {
            if (--p->traceCount <= 0) {
              p->traceCount = 0;
              p->tracing = True;
              return True;
            }
            return False;
          }
          case dCall:
          case dOCall:
            if (p->traceCount > 0) {
              p->traceCount++;
            }
            return False;
          case dTail:
          case dOTail:
            return False;
          default:
            return True;
        }
      }
      case nextSucc: {
        switch (*pc) {
          case dRet: {
            if (--p->traceCount <= 0) {
              p->traceCount = 0;
              p->tracing = True;
              return True;
            }
            return False;
          }
          case dCall:
          case dOCall:
            if (p->traceCount > 0) {
              p->traceCount++;
            }
            return False;
          default:
            return False;
        }
      }
      case nextBreak: {
        switch (*pc) {
          case dCall:
          case dTail: {
            labelPo callee = C_LBL(getMtdLit(mtd, collect32(pc + 1)));
            if (callBreakPointHit(callee)) {
              p->waitFor = stepInto;
              p->tracing = True;
              return True;
            } else
              return False;
          }
          case dLine: {
            normalPo ln = C_TERM(getMtdLit(mtd, collect32(pc + 1)));
            if (lineBreakPointHit(ln)) {
              p->waitFor = stepInto;
              p->tracing = True;
              return True;
            }
            return False;
          }
          default:
            return False;
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
  p->traceCount = cmdCount(line, 1);
  return stepInto;
}

static DebugWaitFor dbgOver(char *line, processPo p, void *cl) {
  p->traceCount = cmdCount(line, 1);
  return stepOver;
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

static DebugWaitFor dbgUntilRet(char *line, processPo p, void *cl) {
  p->traceCount = cmdCount(line, 1);
  p->tracing = False;
  return nextSucc;
}

static DebugWaitFor dbgShowRegisters(char *line, processPo p, void *cl) {
  showRegisters(p, p->heap, p->prog, p->pc, p->fp, p->sp);
  return moreDebug;
}

static DebugWaitFor dbgShowArg(char *line, processPo p, void *cl) {
  integer argNo = cmdCount(line, 0);
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
  integer lclNo = cmdCount(line, 0);
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;

  if (lclNo == 0)
    showAllLocals(stdErr, mtd, p->pc, fp);
  else if (lclNo > 0 && lclNo <= lclCount(mtd))
    showLcl(stdErr, cmdCount(line, 0), mtd, fp, sp);
  else
    outMsg(stdErr, "invalid local number: %d", lclNo);
  return moreDebug;
}

static DebugWaitFor dbgShowGlobal(char *line, processPo p, void *cl) {
  char buff[MAX_SYMB_LEN];
  integer pos = 0;
  integer ix = 0;
  integer llen = uniStrLen(line);

  enum {
    initSte, inVar
  } st = initSte;

  while (ix < llen) {
    codePoint cp = nextCodePoint(line, &ix, llen);
    switch (st) {
      case initSte:
        if (!isSpaceChar(cp)) {
          st = inVar;
          appendCodePoint(buff, &pos, NumberOf(buff), cp);
        }
        continue;
      case inVar:
        if (!isSpaceChar(cp)) {
          appendCodePoint(buff, &pos, NumberOf(buff), cp);
          continue;
        } else
          break;
    }
  }
  if (uniStrLen(buff) > 0) {
    appendCodePoint(buff, &pos, NumberOf(buff), 0);
    globalPo glb = globalVar(buff);
    if (glb != Null) {
      termPo val = getGlobal(glb);
      if (val != Null)
        outMsg(stdErr, "%s = %T\n", buff, val);
      else
        outMsg(stdErr, "%s not set\n", buff);
    }
  }
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
    showStack(stdErr, p, mtd, cmdCount(line, 1), fp, sp);

  return moreDebug;
}

void showStackEntry(ioPo out, integer frameNo, methodPo mtd, insPo pc, framePo fp, ptrPo sp, logical showStack) {
  integer pcOffset = (integer) (pc - mtd->code);
  outMsg(out, "[%d] %T[%d](", frameNo, mtd, pcOffset);

  integer count = argCount(mtd);
  char *sep = "";
  for (integer ix = 0; ix < count; ix++) {
    outMsg(out, "%s%T", sep, fp->args[ix]);
    sep = ", ";
  }
  outMsg(out, ")\n");

  showAllLocals(out, mtd, pc, fp);

  if (showStack) {
    ptrPo stackTop = ((ptrPo) fp) - mtd->lclcnt;
    for (integer ix = 0; sp < stackTop; ix++, sp++) {
      termPo t = *sp;
      outMsg(out, "SP[%d]=%T\n", ix, t);
    }
  }
}

static DebugWaitFor dbgStackTrace(char *line, processPo p, void *cl) {
  logical showStack = (logical) (cl);
  heapPo h = processHeap(p);
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;
  insPo pc = p->pc;
  ioPo out = stdErr;

  integer frameNo = 0;

  outMsg(out, "stack trace for p: 0x%x", p);
  stackSummary(out,p,sp);
  heapSummary(out, h);
  outMsg(out, "\n");

  while (sp < (ptrPo) p->stackLimit) {
    showStackEntry(out, frameNo, mtd, pc, fp, sp, showStack);

    mtd = fp->prog;
    pc = fp->rtn;
    sp = (ptrPo) (fp + 1);
    fp = fp->fp;
    frameNo++;
  }
  flushFile(out);

  return moreDebug;
}

static DebugWaitFor dbgShowCode(char *line, processPo p, void *cl) {
  integer count = maximum(1, cmdCount(line, 1));
  methodPo mtd = p->prog;
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
  return stepInto;
}

static DebugWaitFor dbgSymbolDebug(char *line, processPo p, void *cl) {
  lineDebugging = True;
  insDebugging = False;
  return stepInto;
}

static DebugWaitFor dbgDropFrame(char *line, processPo p, void *cl) {
  integer count = cmdCount(line, 0);

  // First we check that there are enough frames
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;

  integer frameNo = 0;

  while (frameNo < count && sp < (ptrPo) p->stackLimit) {
    mtd = fp->prog;
    sp = (ptrPo) (fp + 1);
    fp = fp->fp;
    frameNo++;
  }

  if (sp < (ptrPo) p->stackLimit) {
    p->prog = mtd;
    p->pc = entryPoint(mtd);

    integer lclCnt = lclCount(mtd);  /* How many locals do we have */
    p->sp = sp = ((ptrPo) fp) - lclCnt;
    p->fp = fp;

#ifdef TRACEEXEC
    for (integer ix = 0; ix < lclCnt; ix++)
      sp[ix] = voidEnum;
#endif
  } else
    outMsg(logFile, "Could not drop %d stack frame\n%_", count);

  return moreDebug;
}

DebugWaitFor insDebug(integer pcCount, processPo p) {
  static DebugOption opts[] = {
    {.c = 'n', .cmd=dbgSingle, .usage="n step into"},
    {.c = '\n', .cmd=dbgSingle, .usage="\\n step into"},
    {.c = 'N', .cmd=dbgOver, .usage="N step over"},
    {.c = 'q', .cmd=dbgQuit, .usage="q stop execution"},
    {.c = 't', .cmd=dbgTrace, .usage="t trace mode"},
    {.c = 'c', .cmd=dbgCont, .usage="c continue"},
    {.c = 'u', .cmd=dbgUntilRet, .usage="u <count> until next <count> returns"},
    {.c = 'r', .cmd=dbgShowRegisters, .usage="r show registers"},
    {.c = 'l', .cmd=dbgShowLocal, .usage="l show local variable"},
    {.c = 'a', .cmd=dbgShowArg, .usage="a show argument"},
    {.c = 's', .cmd=dbgShowStack, .usage="s show stack"},
    {.c = 'S', .cmd=dbgStackTrace, .usage="S show entire stack"},
    {.c = 'D', .cmd=dbgDropFrame, .usage="D <count> drop stack frame(s)"},
    {.c = 'g', .cmd=dbgShowGlobal, .usage="g <var> show global var"},
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
        if (p->traceCount == 0)
          p->waitFor = cmder(opts, NumberOf(opts), p, h, mtd, pc, fp, sp);
        else {
          outStr(stdErr, "\n");
          flushFile(stdErr);
        }

        switch (p->waitFor) {
          case moreDebug:
            continue;
          case stepInto:
          case stepOver:
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

void showLn(ioPo out, methodPo mtd, termPo ln, framePo fp, ptrPo sp) {
  outMsg(out, "line: %L%_", ln);
}

retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt) {
  termPo ln = (termPo) data;

  if (isNormalPo(ln)) {
    normalPo line = C_TERM(ln);
    integer pLen;
    const char *pkgNm = stringVal(nthArg(line, 0), &pLen);
    return outMsg(f, "%S/%T:%T(%T)", pkgNm, pLen, nthArg(line, 1), nthArg(line, 2), nthArg(line, 4));
  } else
    return outMsg(f, "%T", ln);
}

static retCode shCall(ioPo out, char *msg, methodPo mtd, framePo fp, ptrPo sp) {
  tryRet(outMsg(out, "%s%#.16T(", msg, mtd));
  integer count = argCount(mtd);
  char *sep = "";
  for (integer ix = 0; ix < count; ix++) {
    tryRet(outMsg(out, "%s%#T", sep, sp[ix]));
    sep = ", ";
  }
  return outMsg(out, ")");
}

void showCall(ioPo out, methodPo mtd, termPo call, framePo fp, ptrPo sp) {
  shCall(out, "call: ", labelCode(C_LBL(call)), fp, sp);
}

void showTail(ioPo out, methodPo mtd, termPo call, framePo fp, ptrPo sp) {
  shCall(out, "tail: ", labelCode(C_LBL(call)), fp, sp);
}

void showRet(ioPo out, methodPo mtd, termPo val, framePo fp, ptrPo sp) {
  outMsg(out, "return: %T->%,10T", mtd, val);
}

typedef void (*showCmd)(ioPo out, methodPo mtd, termPo trm, framePo fp, ptrPo sp);

termPo getLbl(termPo lbl, int32 arity) {
  labelPo oLbl = isNormalPo(lbl) ? termLbl(C_TERM(lbl)) : C_LBL(lbl);
  return (termPo) declareLbl(oLbl->name, arity);
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
  return lnDebug(p, ln, showLn);
}

DebugWaitFor lnDebug(processPo p, termPo ln, showCmd show) {
  static DebugOption opts[] = {
    {.c = 'n', .cmd=dbgSingle, .usage="n step into"},
    {.c = '\n', .cmd=dbgSingle, .usage="\\n step into"},
    {.c = 'N', .cmd=dbgOver, .usage="N step over"},
    {.c = 'q', .cmd=dbgQuit, .usage="q stop execution"},
    {.c = 't', .cmd=dbgTrace, .usage="t trace mode"},
    {.c = 'c', .cmd=dbgCont, .usage="c continue"},
    {.c = 'u', .cmd=dbgUntilRet, .usage="u <count> until next <count> returns"},
    {.c = 'r', .cmd=dbgShowRegisters, .usage="r show registers"},
    {.c = 'a', .cmd=dbgShowArg, .usage="a show argument"},
    {.c = 's', .cmd=dbgShowStack, .usage="s show stack", .cl=(void *) False},
    {.c = 'S', .cmd=dbgStackTrace, .usage="S show entire stack"},
    {.c = 'D', .cmd=dbgDropFrame, .usage="D <count> drop stack frame(s)"},
    {.c = 'g', .cmd=dbgShowGlobal, .usage="g <var> show global var"},
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
      show(logFile, mtd, ln, fp, sp);
    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0)
          p->waitFor = cmder(opts, NumberOf(opts), p, h, mtd, pc, fp, sp);
        else {
          p->traceCount--;
          outStr(stdErr, "\n");
          flushFile(stdErr);
        }

        switch (p->waitFor) {
          case moreDebug:
            continue;
          case stepInto:
          case stepOver:
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

void stackSummary(ioPo out,processPo P,ptrPo sp){
  outMsg(out,", sp: 0x%x, stack:%5.2g%%",sp,(sp-(ptrPo)P->stackBase)*100.0/(P->stackLimit-P->stackBase));
}

void showAllArgs(ioPo out, processPo p, methodPo mtd, framePo fp, ptrPo sp) {
  integer count = argCount(mtd);
  for (integer ix = 0; ix < count; ix++) {
    outMsg(out, "A[%d] = %T\n", ix + 1, fp->args[ix]);
  }
}

retCode showArg(ioPo out, integer arg, methodPo mtd, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null)
    return outMsg(out, " a[%d] = %T", arg, fp->args[arg - 1]);
  else
    return outMsg(out, " a[%d]", arg);
}

void showAllLocals(ioPo out, methodPo mtd, insPo pc, framePo fp) {
  for (integer vx = 1; vx <= lclCount(mtd); vx++) {
    char vName[MAX_SYMB_LEN];
    if (localVName(mtd, pc, vx, vName, NumberOf(vName)) == Ok) {
      ptrPo var = localVar(fp, vx);
      if (*var != Null)
        outMsg(out, "  %s(%d) = %T\n", vName, vx, *var);
      else
        outMsg(out, "  %s(%d) (unset)", vName, vx);
    }
  }
}

retCode showLcl(ioPo out, integer vr, methodPo mtd, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null) {
    ptrPo var = localVar(fp, vr);
    if (*var != Null)
      return outMsg(out, " l[%d] = %T", vr, *var);
    else
      return outMsg(out, " l[%d] (unset)", vr);
  } else
    return outMsg(out, " l[%d]", vr);
}

retCode showGlb(ioPo out, globalPo glb, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null) {
    if (glb != Null) {
      termPo val = getGlobal(glb);
      if (val != Null)
        return outMsg(out, " %s = %T", globalVarName(glb), val);
      else
        return outMsg(out, " %s", globalVarName(glb));
    } else
      return outMsg(out, "unknown global");
  } else
    return outMsg(out, " %s", globalVarName(glb));
}

retCode showTos(ioPo out, framePo fp, ptrPo sp) {
  if (sp != Null)
    return outMsg(out, " <tos> = %,10T", sp[0]);
  else
    return outMsg(out, " <tos>");
}

void showAllStack(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  ptrPo stackTop = ((ptrPo) fp) - lclCount(mtd);

  for (integer ix = 0; sp < stackTop; ix++, sp++) {
    outMsg(out, "SP[%d]=%T\n", ix, *sp);
  }
}

void showStack(ioPo out, processPo p, methodPo mtd, integer vr, framePo fp, ptrPo sp) {
  ptrPo stackTop = ((ptrPo) fp) - lclCount(mtd);

  if (vr >= 0 && vr < stackTop - sp)
    outMsg(out, "SP[%d]=%,10T\n", vr, sp[vr]);
  else
    outMsg(out, "invalid stack offset: %d", vr);
}

insPo disass(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  int32 hi32, lo32;

  integer offset = (integer) (pc - entryPoint(mtd));

  normalPo lits = codeLits(mtd);
  if (lits != Null)
    outMsg(out, "0x%x: %T(%d) ", pc, nthArg(codeLits(mtd), 0), offset);
  else
    outMsg(out, "0x%x: \?\?(%d) ", pc, offset);

  switch (*pc++) {
#undef instruction

#define show_nOp
#define show_tOs showTos(out,fp,sp)
#define show_i32 outMsg(out," #%d",collectI32(pc))
#define show_arg showArg(out,collectI32(pc),mtd,fp,sp)
#define show_lcl showLcl(out,collectI32(pc),mtd,fp,sp)
#define show_lcs outMsg(out," l[%d]",collectI32(pc))
#define show_off outMsg(out," PC[%d]",collectI32(pc))
#define show_Es outMsg(out, " %s", getEscape(collectI32(pc))->name)
#define show_lit showConstant(out,mtd,collectI32(pc))
#define show_glb showGlb(out, getGlobalVar(collectI32(pc)),fp,sp)

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

  outMsg(logFile, "p: 0x%x, mtd: %T[%d], pc: 0x%x, fp: 0x%x", p, mtd, pcOffset, pc, fp);
  stackSummary(logFile,p,sp);
  heapSummary(logFile, h);
  outMsg(logFile, "\n");

  ptrPo stackTop = ((ptrPo) fp) - mtd->lclcnt;

  showAllLocals(logFile, mtd, pc, fp);
  showAllArgs(logFile, p, mtd, fp, sp);

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

  logMsg(logFile, "%d instructions executed\n", pcCount);
}
