// Incremental instruction debugger

#include "engineP.h"
#include <stdlib.h>
#include <globals.h>
#include <str.h>
#include <ioTcp.h>
#include <manifest.h>

#include "debugP.h"
#include "arith.h"
#include "editline.h"

integer pcCount = 0;

static void showCall(ioPo out, methodPo mtd, insPo pc, termPo call, framePo fp, ptrPo sp);
static void showTail(ioPo out, methodPo mtd, insPo pc, termPo call, framePo fp, ptrPo sp);
static void showOCall(ioPo out, methodPo mtd, insPo pc, termPo call, framePo fp, ptrPo sp);
static void showOTail(ioPo out, methodPo mtd, insPo pc, termPo call, framePo fp, ptrPo sp);
static void showLn(ioPo out, methodPo mtd, insPo pc, termPo ln, framePo fp, ptrPo sp);
static void showRet(ioPo out, methodPo mtd, insPo pc, termPo val, framePo fp, ptrPo sp);

static void showRegisters(processPo p, heapPo h, methodPo mtd, insPo pc, framePo fp, ptrPo sp);
static void showAllLocals(ioPo out, methodPo mtd, insPo pc, framePo fp);
static retCode showTos(ioPo out, framePo fp, ptrPo sp);
static retCode showLcl(ioPo out, integer vr, methodPo mtd, framePo fp, ptrPo sp);
static retCode showArg(ioPo out, integer arg, methodPo mtd, framePo fp, ptrPo sp);
static void showAllArgs(ioPo out, processPo p, methodPo mtd, framePo fp, ptrPo sp);
static void showAllStack(ioPo out, processPo p, methodPo mtd, framePo fp, ptrPo sp);
static void showStack(ioPo out, processPo p, methodPo mtd, integer vr, framePo fp, ptrPo sp);
static void showStackCall(ioPo out, integer frameNo, methodPo mtd, insPo pc, framePo fp, integer displayDepth);
static retCode localVName(methodPo mtd, insPo pc, integer vNo, char *buffer, integer bufLen);
static void stackSummary(ioPo out, processPo P, ptrPo sp);

static insPo disass(ioPo out, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp);

static sockPo debuggerListener = Null;

static ioPo debugInChnnl = Null;
static ioPo debugOutChnnl = Null;

retCode setupDebugChannels() {
  if (debuggerPort > 0 && debugInChnnl == Null) {
    if (debuggerListener == Null) {
      debuggerListener = listeningPort("star-debug", debuggerPort);
      showPkgFile = True;

      retCode ret = acceptConnection(debuggerListener, utf8Encoding, &debugInChnnl, &debugOutChnnl);
      if (ret != Ok) {
        syserr("fatal problem in establishing debugger connection");
      }
      return ret;
    }
  } else if (debugInChnnl == Null) {
    debugInChnnl = Stdin();
    debugOutChnnl = Stdout();
    return Ok;
  }
  return Error;
}

static inline int32 collect32(insPo pc) {
  uint32 hi = (uint32) pc[0];
  uint32 lo = (uint32) pc[1];
  return (int32) (hi << (uint32) 16 | lo);
}

#define collectI32(pc) (collI32(pc))
#define collI32(pc) hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<16)|lo32)

ReturnStatus g__ins_debug(processPo P, ptrPo a) {
  insDebugging = tracing = True;
  P->waitFor = stepInto;
  P->tracing = True;
  P->traceCount = 0;
  P->traceDepth = 0;

  return rtnStatus(P, Ok, "_ins_debug");
}

static integer cmdCount(char *cmdLine, integer deflt) {
  while (isSpaceChar((codePoint) *cmdLine))
    cmdLine++;
  if (uniStrLen(cmdLine) == 0)
    return deflt;
  else
    return parseInteger(cmdLine, uniStrLen(cmdLine));
}

static processPo focus = NULL;
static pthread_mutex_t debugMutex = PTHREAD_MUTEX_INITIALIZER;

integer displayDepth = 1;

void dC(termPo w) {
  outMsg(logFile, "%,*T\n", displayDepth, w);
  flushOut();
}

static retCode showConstant(ioPo out, methodPo mtd, integer off) {
  return outMsg(out, " %,*T", displayDepth, nthArg(mtd->pool, off));
}

static logical shouldWeStop(processPo p, insWord ins, termPo arg) {
  if (focus == NULL || focus == p) {
    if (debugDebugging) {
      outMsg(logFile, "debug: traceDepth=%d, traceCount=%d, tracing=%s, ins: ", p->traceDepth, p->traceCount,
             (p->tracing ? "yes" : "no"));
      disass(logFile, p, p->prog, p->pc, p->fp, p->sp);
      outMsg(logFile, "\n%_");
    }
    switch (ins) {
      case Ret: {
        switch (p->waitFor) {
          case stepInto:
            return True;
          case stepOver:
            if (p->traceDepth > 0)
              p->traceDepth--;
            return (logical) (p->traceDepth == 0 && p->traceCount == 0);
          default:
            return False;
        }
      }
      case Call:
      case OCall: {
        breakPointPo bp = callBreakPointHit(C_LBL(arg));
        if (bp != Null) {
          p->waitFor = stepInto;
          p->tracing = True;
          p->traceDepth = p->traceCount = 0;
          if (isTempBreakPoint(bp))
            clearBreakPoint(bp);
          return True;
        } else {
          switch (p->waitFor) {
            case stepInto:
              return True;
            case stepOver:
              p->traceDepth++;
              return (logical) (p->traceCount == 0 && p->traceDepth == 1);
            case nextBreak:
            default:
              return False;
          }
        }
      }
      case Tail:
      case OTail: {
        breakPointPo bp = callBreakPointHit(C_LBL(arg));

        if (bp != Null) {
          p->waitFor = stepInto;
          p->tracing = True;
          p->traceDepth = p->traceCount = 0;
          if (isTempBreakPoint(bp))
            clearBreakPoint(bp);
          return True;
        } else
          switch (p->waitFor) {
            case stepOver:
            case stepInto:
              return (logical) (p->traceDepth == 0 && p->traceCount == 0);
            case nextBreak:
            default:
              return False;
          }
      }

      case dLine: {
        breakPointPo bp = lineBreakPointHit(C_TERM(arg));
        if (bp != Null) {
          p->waitFor = stepInto;
          p->tracing = True;
          p->traceDepth = p->traceCount = 0;
          if (isTempBreakPoint(bp))
            clearBreakPoint(bp);
          return True;
        } else
          switch (p->waitFor) {
            case stepInto:
              return True;
            case stepOver:
              if (p->traceDepth == 0) {
                if (p->traceCount > 0)
                  p->traceCount--;
                return (logical) (p->traceCount == 0);
              } else
                return False;
            default:
              return False;
          }
      }

      default:
        return False;
    }
  } else
    return False;
}

static char *defltLine(char *src, integer srcLen, integer *actLen) {
  static char defltLn[MAXLINE] = {'n'};

  if (!uniIsTrivial(src, srcLen)) {
    uniNCpy(defltLn, NumberOf(defltLn), src, srcLen);
    *actLen = srcLen;
    return src;
  } else {
    *actLen = uniNStrLen(defltLn, NumberOf(defltLn));
    return defltLn;
  }
}

static void resetDeflt(char *cmd) {
  integer junk;
  defltLine(cmd, uniStrLen(cmd), &junk);
}

static retCode cmdComplete(bufferPo b, void *cl, integer cx) {
  integer bLen;
  char *content = getTextFromBuffer(b, &bLen);

  if (bLen == 0)
    return Eof;
  else {
    integer pos = 0;
    codePoint first = nextCodePoint(content, &pos, bLen);

    debugOptPo opts = (debugOptPo) cl;
    for (int ix = 0; ix < opts->count; ix++) {
      if (opts->opts[ix].c == first) {
        if (opts->opts[ix].complete != Null)
          return opts->opts[ix].complete(b, cx);
        else
          return Eof;
      }
    }
    if (opts->deflt != Null)
      return opts->deflt(b, cx);
    else
      return Eof;
  }
}

static void dbgPrompt(processPo p) {
  outMsg(debugOutChnnl, "\n[%d]>>%_", processNo(p));
}

static DebugWaitFor cmder(debugOptPo opts, processPo p, methodPo mtd, termPo loc, insWord ins) {
  static bufferPo cmdBuffer = Null;

  if (cmdBuffer == Null)
    cmdBuffer = newStringBuffer();

  while (interactive) {
    dbgPrompt(p);
    clearBuffer(cmdBuffer);

    setEditLineCompletionCallback(cmdComplete, (void *) opts);
    retCode res = (debuggerListener == Null ? consoleInput(cmdBuffer) : inLine(debugInChnnl, cmdBuffer, "\n"));
    clearEditLineCompletionCallback();

    switch (res) {
      case Eof:
        return quitDbg;
      case Ok: {
        integer cmdLen = 0;
        char *cmdLine = getTextFromBuffer(cmdBuffer, &cmdLen);

        cmdLine = defltLine(cmdLine, cmdLen, &cmdLen);

        codePoint cmd;
        integer nxt = 0;
        cmd = nextCodePoint(cmdLine, &nxt, cmdLen);

        if (isNdChar((codePoint) cmd)) {
          cmd = 'n';
          nxt = 0;
        }

        for (int ix = 0; ix < opts->count; ix++) {
          if (opts->opts[ix].c == cmd)
            return opts->opts[ix].cmd(&cmdLine[nxt], p, loc, ins, opts->opts[ix].cl);
        }
        outMsg(debugOutChnnl, "invalid debugger command: %s\n", cmdLine);
      }
      default:
        for (int ix = 0; ix < opts->count; ix++)
          outMsg(debugOutChnnl, "%s\n", opts->opts[ix].usage);
        flushFile(debugOutChnnl);
        return moreDebug;
    }
  }
  return moreDebug;
}

static DebugWaitFor dbgSingle(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->traceDepth = 0;
  p->tracing = (logical) (p->traceCount == 0);
  return stepInto;
}

static DebugWaitFor dbgOver(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  p->traceCount = cmdCount(line, 0);

  switch (ins) {
    case Ret: {
      p->traceDepth = 0;
      p->tracing = True;
      return stepOver;
    }
    case Call:
    case OCall: {
      p->traceDepth = 1;
      p->tracing = False;
      return stepOver;
    }
    case Tail:
    case OTail: {
      p->traceDepth = 0;
      p->tracing = False;
      return stepOver;
    }

    default:
      p->traceDepth = 0;
      return stepOver;
  }
}

static DebugWaitFor dbgQuit(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  return quitDbg;
}

static DebugWaitFor dbgTrace(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  p->tracing = True;

  resetDeflt("n");
  return nextBreak;
}

static DebugWaitFor dbgCont(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  p->tracing = False;

  resetDeflt("n");
  return nextBreak;
}

static DebugWaitFor dbgUntilRet(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->tracing = False;
  resetDeflt("n");

  switch (ins) {
    case Ret: {
      p->traceDepth = 1;
      return stepOver;
    }
    case Call:
    case OCall: {
      p->traceDepth = 2;
      return stepOver;
    }
    case Tail:
    case OTail: {
      p->traceDepth = 1;
      return stepOver;
    }

    default:
      p->traceDepth = 1;
      return stepOver;
  }
}

static DebugWaitFor dbgSetDepth(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  displayDepth = cmdCount(line, 0);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowRegisters(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  showRegisters(p, p->heap, p->prog, p->pc, p->fp, p->sp);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowArg(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  integer argNo = cmdCount(line, 0);
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;

  if (argNo == 0)
    showAllArgs(debugOutChnnl, p, mtd, fp, sp);
  else if (argNo > 0 && argNo < argCount(mtd))
    showArg(debugOutChnnl, argNo, mtd, fp, sp);
  else
    outMsg(debugOutChnnl, "invalid argument number: %d", argNo);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowLocal(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  integer lclNo = cmdCount(line, 0);
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;

  if (lclNo == 0)
    showAllLocals(debugOutChnnl, mtd, p->pc, fp);
  else if (lclNo > 0 && lclNo <= lclCount(mtd))
    showLcl(debugOutChnnl, cmdCount(line, 0), mtd, fp, sp);
  else
    outMsg(debugOutChnnl, "invalid local number: %d", lclNo);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowGlobal(char *line, processPo p, termPo loc, insWord ins, void *cl) {
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
        outMsg(debugOutChnnl, "%s = %,*T\n", buff, displayDepth, val);
      else
        outMsg(debugOutChnnl, "%s not set\n", buff);
    }
  }

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowStack(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;

  if (line[0] == '\n') {
    showAllStack(debugOutChnnl, p, mtd, fp, sp);
  } else
    showStack(debugOutChnnl, p, mtd, cmdCount(line, 1), fp, sp);

  resetDeflt("n");
  return moreDebug;
}

void showStackCall(ioPo out, integer frameNo, methodPo mtd, insPo pc, framePo fp, integer displayDepth) {
  integer pcOffset = (integer) (pc - mtd->code);

  termPo locn = findPcLocation(mtd, pcOffset);
  if (locn != Null)
    outMsg(out, "[%d] %#L: %T(", frameNo, locn, mtd);
  else
    outMsg(out, "[%d] (unknown loc): %T[%d](", frameNo, mtd, pcOffset);

  integer count = argCount(mtd);
  char *sep = "";
  for (integer ix = 0; ix < count; ix++) {
    outMsg(out, "%s%,*T", sep, displayDepth, fp->args[ix]);
    sep = ", ";
  }
  outMsg(out, ")\n");
}

void showStackEntry(ioPo out, integer frameNo, methodPo mtd, insPo pc, framePo fp, ptrPo sp, logical showStack) {
  showStackCall(out, frameNo, mtd, pc, fp, displayDepth);
  showAllLocals(out, mtd, pc, fp);

  if (showStack) {
    ptrPo stackTop = ((ptrPo) fp) - mtd->lclcnt;
    for (integer ix = 0; sp < stackTop; ix++, sp++) {
      termPo t = *sp;
      outMsg(out, "SP[%d]=%,*T\n", ix, displayDepth, t);
    }
  }
}

static DebugWaitFor dbgStackTrace(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  stackTrace(p, debugOutChnnl, (logical) (cl));

  resetDeflt("n");
  return moreDebug;
}

void stackTrace(processPo p, ioPo out, logical showStack) {
  heapPo h = processHeap(p);
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;
  insPo pc = p->pc;

  integer frameNo = 0;

  outMsg(out, "Stack trace for process %d ", p->processNo);
#ifdef TRACEEXEC
  if (debugDebugging) {
    stackSummary(out, p, sp);
    heapSummary(out, h);
  }
#endif
  outMsg(out, "\n%_");

  while (fp < (framePo) p->stackLimit) {
    showStackEntry(out, frameNo, mtd, pc, fp, sp, showStack);

    mtd = fp->prog;
    pc = fp->rtn;
    sp = (ptrPo) (fp + 1);
    fp = fp->fp;
    frameNo++;
    assert(fp <= (framePo) p->stackLimit && fp >= (framePo) p->stackBase);
  }
  flushFile(out);
}

void dumpStackTrace(processPo p, ioPo out) {
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  insPo pc = p->pc;

  integer frameNo = 0;

  outMsg(out, "Stack dump for p: %d\n", processNo(p));

  while (fp < (framePo) p->stackLimit) {
    showStackCall(out, frameNo, mtd, pc, fp, displayDepth);

    mtd = fp->prog;
    pc = fp->rtn;
    fp = fp->fp;
    frameNo++;

    assert(fp <= (framePo) p->stackLimit && fp >= (framePo) p->stackBase);
  }
  flushFile(out);
}

static DebugWaitFor dbgShowCode(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  integer count = maximum(1, cmdCount(line, 1));
  methodPo mtd = p->prog;
  insPo pc = p->pc;

  insPo last = entryPoint(mtd) + insCount(mtd);

  for (integer ix = 0; ix < count && pc < last; ix++) {
    pc = disass(debugOutChnnl, p, mtd, pc, Null, Null);
    outStr(debugOutChnnl, "\n");
  }

  flushFile(debugOutChnnl);
  resetDeflt("n");

  return moreDebug;
}

static DebugWaitFor dbgInsDebug(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  lineDebugging = False;
  insDebugging = True;
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgSymbolDebug(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  lineDebugging = True;
  insDebugging = False;
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgAddBreakPoint(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok)
    ret = isValidBreakPoint(&bp);
  if (ret == Ok)
    ret = addBreakPoint(&bp);
  if (ret != Ok) {
    outMsg(debugOutChnnl, "Could not set spy point on %s\n", line);
    outMsg(debugOutChnnl, "usage: +pkg/Arity or +pkg:LineNo\n%_");
  } else
    outMsg(debugOutChnnl, "%sspy point set on %s\n%_", bp.bkType == callBreak ? "call " : "line ", line);
  return moreDebug;
}

DebugWaitFor dbgClearBreakPoint(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok)
    ret = clearBreakPoint(&bp);
  if (ret != Ok)
    outMsg(debugOutChnnl, "Could not clear spy point on %s\n%_", line);
  else
    outMsg(debugOutChnnl, "spy point cleared on %s\n%_", line);
  return moreDebug;
}

DebugWaitFor dbgShowBreakPoints(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  retCode ret = showAllBreakPoints(debugOutChnnl);

  if (ret != Ok)
    outMsg(debugOutChnnl, "Could not show break points\n%_");
  return moreDebug;
}

static DebugWaitFor dbgDropFrame(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  integer count = cmdCount(line, 0);

  // First we check that there are enough frames
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;

  integer frameNo = 0;

  while (frameNo < count && fp < (framePo) p->stackLimit) {
    mtd = fp->prog;
    sp = (ptrPo) (fp + 1);
    fp = fp->fp;
    frameNo++;
  }

  if (fp < (framePo) p->stackLimit) {
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
    outMsg(debugOutChnnl, "Could not drop %d stack frame\n%_", count);

  resetDeflt("n");
  return moreDebug;
}

static logical shouldWeStopIns(processPo p, insWord ins) {
  if (focus == NULL || focus == p) {
    if (debugDebugging) {
      outMsg(logFile, "debug: traceDepth=%d, traceCount=%d, tracing=%s, ins: ", p->traceDepth, p->traceCount,
             (p->tracing ? "yes" : "no"));
      disass(logFile, p, p->prog, p->pc, p->fp, p->sp);
      outMsg(logFile, "\n%_");
    }
    switch (p->waitFor) {
      case stepInto:
        if (p->traceCount > 0)
          p->traceCount--;
        return (logical) (p->traceCount == 0);

      case stepOver:
      case nextBreak:
        break;
      default:
        return False;
    }

    switch (ins) {
      case Ret: {
        switch (p->waitFor) {
          case stepOver:
            if (p->traceDepth > 0)
              p->traceDepth--;
            return (logical) (p->traceDepth == 0 && p->traceCount == 0);
          default:
            return False;
        }
      }
      case Call:
      case OCall: {
        switch (p->waitFor) {
          case stepOver:
            p->traceDepth++;
            return (logical) (p->traceCount == 0 && p->traceDepth == 1);
          default:
            return False;
        }
      }
      case Tail:
      case OTail:
        switch (p->waitFor) {
          case stepOver:
            return (logical) (p->traceDepth == 0 && p->traceCount == 0);
          default:
            return False;
        }
      case dLine: {
        termPo loc = findPcLocation(p->prog, insOffset(p->prog, p->pc));

        breakPointPo bp = lineBreakPointHit(C_TERM(loc));
        if (bp != Null) {
          p->waitFor = stepInto;
          p->tracing = True;
          p->traceDepth = p->traceCount = 0;
          if (isTempBreakPoint(bp))
            clearBreakPoint(bp);
          return True;
        } else
          return False;
      }

      default:
        return False;
    }
  } else
    return False;
}

DebugWaitFor insDebug(processPo p, insWord ins) {
  static DebugOptions opts = {
    .opts = {
      {.c = 'n', .cmd=dbgSingle, .usage="n step into"},
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
      {.c = 'd', .cmd=dbgSetDepth, .usage="d <dpth> set display depth"},
      {.c = '+', .cmd=dbgAddBreakPoint, .usage="+ add break point"},
      {.c = '-', .cmd=dbgClearBreakPoint, .usage="- clear break point"},
      {.c = 'B', .cmd=dbgShowBreakPoints, .usage="show all break points"},
      {.c = 'y', .cmd=dbgSymbolDebug, .usage="y turn on symbolic mode"}},
    .count = 18,
    .deflt = Null
  };

  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;
  insPo pc = p->pc;

  logical stopping = shouldWeStopIns(p, ins);
  if (p->tracing || stopping) {
    outMsg(debugOutChnnl, "[%d]: ", pcCount);
    disass(debugOutChnnl, p, mtd, pc, fp, sp);

    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0)
          p->waitFor = cmder(&opts, p, mtd, Null, ins);
        else {
          outStr(debugOutChnnl, "\n");
        }

        flushFile(debugOutChnnl);

        switch (p->waitFor) {
          case moreDebug:
            continue;
          case stepInto:
          case stepOver:
          case nextBreak:
          case never:
            return p->waitFor;
          case quitDbg:
            exit(0);
        }
      }
    } else {
      outStr(debugOutChnnl, "\n");
      flushFile(debugOutChnnl);
    }
  }
  return p->waitFor;
}

void showLn(ioPo out, methodPo mtd, insPo pc, termPo ln, framePo fp, ptrPo sp) {
  if (showColors)
    outMsg(out, BLUE_ESC_ON"line:"BLUE_ESC_OFF" %#L%_", ln);
  else
    outMsg(out, "line: %#L%_", ln);
}

retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt) {
  termPo ln = (termPo) data;

  if (isNormalPo(ln)) {
    normalPo line = C_TERM(ln);
    char pkgNm[MAX_SYMB_LEN];
    copyString2Buff(C_STR(nthArg(line, 0)), pkgNm, NumberOf(pkgNm));

    if (alt && showPkgFile) {
      packagePo pkg = loadedPackage(pkgNm);
      char *src = manifestResource(pkg, "source");

      return outMsg(f, "%s(%T:%T@%T,%T)%_", src, nthArg(line, 1), nthArg(line, 2), nthArg(line, 3), nthArg(line, 4));
    } else
      return outMsg(f, "%s:%T:%T(%T)", pkgNm, nthArg(line, 1), nthArg(line, 2), nthArg(line, 4));
  } else
    return outMsg(f, "%,*T", displayDepth, ln);
}

static retCode shArgs(ioPo out, ptrPo sp, integer depth, integer from, integer to) {
  char *sep = "";
  tryRet(outStr(out, "("));
  for (integer ix = from; ix < to; ix++) {
    tryRet(outMsg(out, "%s%#,*T", sep, depth, sp[ix]));
    sep = ", ";
  }
  return outMsg(out, ")");
}

static retCode shCall(ioPo out, char *msg, termPo locn, methodPo mtd, framePo fp, ptrPo sp) {
  if (locn != Null) {
    tryRet(outMsg(out, "%s %#L %#.16T", msg, locn, mtd));
  } else
    tryRet(outMsg(out, "%s %#.16T", msg, mtd));

  return shArgs(out, sp, displayDepth, 0, argCount(mtd));
}

void showCall(ioPo out, methodPo mtd, insPo pc, termPo call, framePo fp, ptrPo sp) {
  termPo locn = findPcLocation(mtd, insOffset(mtd, pc));
  if (showColors)
    shCall(out, GREEN_ESC_ON"call:"GREEN_ESC_OFF, locn, labelCode(C_LBL(call)), fp, sp);
  else
    shCall(out, "call:", locn, labelCode(C_LBL(call)), fp, sp);
}

void showTail(ioPo out, methodPo mtd, insPo pc, termPo call, framePo fp, ptrPo sp) {
  termPo locn = findPcLocation(mtd, insOffset(mtd, pc));
  if (showColors)
    shCall(out, GREEN_ESC_ON"tail:"GREEN_ESC_OFF, locn, labelCode(C_LBL(call)), fp, sp);
  else
    shCall(out, "tail:", locn, labelCode(C_LBL(call)), fp, sp);

}

static retCode shOCall(ioPo out, char *msg, termPo locn, methodPo mtd, framePo fp, ptrPo sp) {
  if (locn != Null) {
    tryRet(outMsg(out, "%s %#L ", msg, locn));
  } else
    tryRet(outMsg(out, "%s ", msg));

  tryRet(outMsg(out, "%#,*T", displayDepth, sp[0]));
  tryRet(outMsg(out, "•%#.16T", mtd));

  return shArgs(out, sp, displayDepth, 1, argCount(mtd));
}

void showOCall(ioPo out, methodPo mtd, insPo pc, termPo call, framePo fp, ptrPo sp) {
  termPo locn = findPcLocation(mtd, insOffset(mtd, pc));
  if (showColors)
    shOCall(out, GREEN_ESC_ON"ocall:"GREEN_ESC_OFF, locn, labelCode(C_LBL(call)), fp, sp);
  else
    shOCall(out, "ocall:", locn, labelCode(C_LBL(call)), fp, sp);
}

void showOTail(ioPo out, methodPo mtd, insPo pc, termPo call, framePo fp, ptrPo sp) {
  termPo locn = findPcLocation(mtd, insOffset(mtd, pc));
  if (showColors)
    shOCall(out, GREEN_ESC_ON"otail:"GREEN_ESC_OFF, locn, labelCode(C_LBL(call)), fp, sp);
  else
    shOCall(out, "otail:", locn, labelCode(C_LBL(call)), fp, sp);

}

void showRet(ioPo out, methodPo mtd, insPo pc, termPo val, framePo fp, ptrPo sp) {
  termPo locn = findPcLocation(mtd, insOffset(mtd, pc));

  if (locn != Null) {
    if (showColors)
      outMsg(out, RED_ESC_ON"return:"RED_ESC_OFF" %#L %T->%#,*T", locn, mtd, displayDepth, val);
    else
      outMsg(out, "return: %#L %T->%#,*T", locn, mtd, displayDepth, val);
  } else
    outMsg(out, "return: %T->%#,*T", mtd, displayDepth, val);
}

typedef void (*showCmd)(ioPo out, methodPo mtd, insPo pc, termPo trm, framePo fp, ptrPo sp);

termPo getLbl(termPo lbl, int32 arity) {
  labelPo oLbl = isNormalPo(lbl) ? termLbl(C_TERM(lbl)) : C_LBL(lbl);
  return (termPo) declareLbl(oLbl->name, arity);
}

static DebugWaitFor lnDebug(processPo p, insWord ins, termPo ln, showCmd show);

DebugWaitFor callDebug(processPo p, termPo call) {
  return lnDebug(p, Call, call, showCall);
}

DebugWaitFor ocallDebug(processPo p, termPo call) {
  return lnDebug(p, OCall, call, showOCall);
}

DebugWaitFor tailDebug(processPo p, termPo call) {
  return lnDebug(p, Tail, call, showTail);
}

DebugWaitFor otailDebug(processPo p, termPo call) {
  return lnDebug(p, OTail, call, showOTail);
}

DebugWaitFor retDebug(processPo p, termPo val) {
  return lnDebug(p, Ret, val, showRet);
}

DebugWaitFor lineDebug(processPo p, termPo line) {
  return lnDebug(p, dLine, line, showLn);
}

DebugWaitFor enterDebug(processPo p) {
  insPo pc = p->pc;
  insWord ins = *pc++;
  switch (ins) {
    case Call:
      return callDebug(p, getMtdLit(p->prog, collect32(pc)));
    case Tail:
      return tailDebug(p, getMtdLit(p->prog, collect32(pc)));
    case OCall: {
      int32 arity = collect32(pc);
      termPo callee = getLbl(p->sp[0], arity);
      return ocallDebug(p, callee);
    }
    case OTail: {
      int32 arity = collect32(pc);
      termPo callee = getLbl(p->sp[0], arity);
      return otailDebug(p, callee);
    }
    case Ret:
      return retDebug(p, p->sp[0]);
    default:
      return stepOver;
  }
}

DebugWaitFor lnDebug(processPo p, insWord ins, termPo ln, showCmd show) {
  static DebugOptions opts = {.opts = {
    {.c = 'n', .cmd=dbgSingle, .usage="n step into"},
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
    {.c = 'd', .cmd=dbgSetDepth, .usage="d <dpth> set display depth"},
    {.c = '+', .cmd=dbgAddBreakPoint, .usage="+ add break point"},
    {.c = '-', .cmd=dbgClearBreakPoint, .usage="- clear break point"},
    {.c = 'B', .cmd=dbgShowBreakPoints, .usage="show all break points"},
    {.c = 'y', .cmd=dbgInsDebug, .usage="y turn on instruction mode"}},
    .count = 20,
    .deflt = Null
  };
  methodPo mtd = p->prog;
  framePo fp = p->fp;
  ptrPo sp = p->sp;
  insPo pc = p->pc;

  logical stopping = shouldWeStop(p, ins, ln);

#ifdef TRACE_DBG
  if (debugDebugging) {
    logMsg(logFile, "traceCount=%d, traceDepth=%d, stopping=%s, tracing=%s", p->traceCount, p->traceDepth,
           (stopping ? "yes" : "no"), (p->tracing ? "yes" : "no"));
  }
#endif
  if (p->tracing || stopping) {
    if (ln != Null)
      show(debugOutChnnl, mtd, pc, ln, fp, sp);
    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0)
          p->waitFor = cmder(&opts, p, mtd, ln, ins);
        else {
          outStr(debugOutChnnl, "\n");
          flushFile(debugOutChnnl);
        }

        switch (p->waitFor) {
          case moreDebug:
            continue;
          case stepInto:
          case stepOver:
          case nextBreak:
          case never:
            return p->waitFor;
          case quitDbg:
            exit(0);
        }
      }
    } else {
      outStr(debugOutChnnl, "\n");
      flushFile(debugOutChnnl);
    }
  }
  return p->waitFor;
}

void stackSummary(ioPo out, processPo P, ptrPo sp) {
  outMsg(out, "sp: 0x%x, stack:%5.2g%%", sp, (sp - (ptrPo) P->stackBase) * 100.0 / (P->stackLimit - P->stackBase));
}

void showAllArgs(ioPo out, processPo p, methodPo mtd, framePo fp, ptrPo sp) {
  integer count = argCount(mtd);
  outMsg(out, "Arguments:\n");
  for (integer ix = 0; ix < count; ix++) {
    outMsg(out, "A[%d] = %,*T\n", ix, displayDepth, fp->args[ix]);
  }
  flushFile(out);
}

retCode showArg(ioPo out, integer arg, methodPo mtd, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null)
    return outMsg(out, " a[%d] = %,*T", arg, displayDepth, fp->args[arg]);
  else
    return outMsg(out, " a[%d]", arg);
}

static ptrPo localVar(framePo fp, int64 off) {
  return &(((ptrPo) fp)[-off]);
}

void showAllLocals(ioPo out, methodPo mtd, insPo pc, framePo fp) {
  for (integer vx = 1; vx <= lclCount(mtd); vx++) {
    char vName[MAX_SYMB_LEN];
    if (localVName(mtd, pc, vx, vName, NumberOf(vName)) == Ok) {
      ptrPo var = localVar(fp, vx);
      if (*var != Null && *var != voidEnum)
        outMsg(out, "  %s = %#,*T\n", vName, displayDepth, *var);
      else
        outMsg(out, "  %s(%d) (unset)\n", vName, vx);
    }
  }
}

retCode showLcl(ioPo out, integer vr, methodPo mtd, framePo fp, ptrPo sp) {
  if (fp != Null && sp != Null) {
    ptrPo var = localVar(fp, vr);
    if (*var != Null)
      return outMsg(out, " l[%d] = %,20T", vr, *var);
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
        return outMsg(out, " %s = %,*T", globalVarName(glb), displayDepth, val);
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

void showAllStack(ioPo out, processPo p, methodPo mtd, framePo fp, ptrPo sp) {
  ptrPo stackTop = ((ptrPo) fp) - lclCount(mtd);

  for (integer ix = 0; sp < stackTop; ix++, sp++) {
    outMsg(out, "SP[%d]=%,*T\n", ix, displayDepth, *sp);
  }
}

void showTopOfStack(ioPo out, processPo p, methodPo mtd, integer cnt, framePo fp, ptrPo sp) {
  ptrPo stackTop = ((ptrPo) fp) - lclCount(mtd);
  char *sep = "";

  outMsg(out, " %s%,*T(", sep, displayDepth, *sp++);

  for (integer ix = 1; ix < cnt && sp < stackTop; ix++, sp++) {
    outMsg(out, "%s%,*T", sep, displayDepth, *sp);
    sep = ", ";
  }

  outMsg(out, ")\n%_");
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
    outMsg(out, "0x%x: %,*T(%d) ", pc, displayDepth, nthArg(codeLits(mtd), 0), offset);
  else
    outMsg(out, "0x%x: \?\?(%d) ", pc, offset);

  switch (*pc++) {
#undef instruction

#define show_nOp
#define show_tOs showTos(out,fp,sp)
#define show_art showTopOfStack(out,p,mtd,collectI32(pc),fp,sp)
#define show_i32 outMsg(out," #%d",collectI32(pc))
#define show_arg showArg(out,collectI32(pc),mtd,fp,sp)
#define show_lcl showLcl(out,collectI32(pc),mtd,fp,sp)
#define show_lcs outMsg(out," l[%d]",collectI32(pc))
#define show_off outMsg(out," PC[%d]",collectI32(pc))
#define show_sym outMsg(out," PC[%d]",collectI32(pc))
#define show_Es outMsg(out, " %s", getEscape(collectI32(pc))->name)
#define show_lit showConstant(out,mtd,collectI32(pc))
#define show_lne showConstant(out,mtd,collectI32(pc))
#define show_glb showGlb(out, findGlobalVar(collectI32(pc)),fp,sp)

#define instruction(Op, A1, Dl, Cmt)    \
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
  showStackEntry(debugOutChnnl, 0, mtd, pc, fp, sp, True);

#ifdef TRACEEXEC
  if (debugDebugging) {
    stackSummary(debugOutChnnl, p, sp);
    heapSummary(debugOutChnnl, h);
  }
#endif
  outMsg(debugOutChnnl, "\n%_");
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
#define instruction(Op, Arg, Dl, Cmt) outMsg(debugOutChnnl,#Op": %d\n",insCounts[Op]);

void dumpInsCount() {
  logMsg(debugOutChnnl, "%d instructions executed\n", pcCount);
}

void dumpInsStats() {
#include "instructions.h"
}

