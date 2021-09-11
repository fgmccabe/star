// Incremental instruction debugger

#include "engineP.h"
#include <stdlib.h>
#include <globals.h>
#include <chars.h>
#include <ioTcp.h>
#include <manifest.h>
#include <termcap.h>
#include <strings.h>

#include "debugP.h"
#include "arith.h"
#include "editline.h"

integer pcCount = 0;

static void showCall(ioPo out, stackPo stk, termPo call);
static void showTail(ioPo out, stackPo stk, termPo call);
static void showOCall(ioPo out, stackPo stk, termPo call);
static void showOTail(ioPo out, stackPo stk, termPo call);
static void showLn(ioPo out, stackPo stk, termPo ln);
static void showRet(ioPo out, stackPo stk, termPo val);

static void showRegisters(processPo p, heapPo h);
static void showAllLocals(ioPo out, stackPo stk, framePo fp);
static retCode showTos(ioPo out, stackPo stk);
static retCode showLcl(ioPo out, stackPo stk, integer vr);
static retCode showArg(ioPo out, stackPo stk, integer arg);
static void showStackCall(ioPo out, stackPo stk, framePo fp, integer frameNo);
static retCode localVName(methodPo mtd, insPo pc, integer vNo, char *buffer, integer bufLen);
static void stackSummary(ioPo out, stackPo stk);

static sockPo debuggerListener = Null;

static ioPo debugInChnnl = Null;
static ioPo debugOutChnnl = Null;

logical insDebugging = False;     // instruction tracing option
logical lineDebugging = False;
logical debugDebugging = False;
logical tracing = False;          /* tracing option */
integer debuggerPort = 0;                // Debug port to establish listener on
logical showPkgFile = False;      // True if we show file names instead of package names
logical showColors = True;        // True if we want to show colored output

logical interactive = False;      /* interaction instruction tracing */

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
#define collI32(pc) hi32 = (uint32)(uint16)(*(pc)++), lo32 = *(pc)++, ((hi32<<16)|lo32)

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
    return parseInt(cmdLine, uniStrLen(cmdLine));
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

static retCode showLTipe(ioPo out, methodPo mtd, integer off) {
  return outMsg(out, " %,*T", displayDepth, nthArg(mtd->pool, off));
}

static logical shouldWeStop(processPo p, stackPo stk, insWord ins, termPo arg) {
  if (focus == NULL || focus == p) {
    framePo frame = currFrame(stk);

    if (debugDebugging) {
      outMsg(logFile, "debug: traceDepth=%d, traceCount=%d, tracing=%s, ins: ", p->traceDepth, p->traceCount,
             (p->tracing ? "yes" : "no"));
      disass(logFile, stk, frame->prog, frame->pc);
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
      case TCall:
      case TOCall: {
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
        breakPointPo bp = lineBreakPointHit(C_NORMAL(arg));
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

static retCode cmdComplete(strBufferPo b, void *cl, integer cx) {
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
  static strBufferPo cmdBuffer = Null;

  if (cmdBuffer == Null)
    cmdBuffer = newStringBuffer();

  while (interactive) {
    dbgPrompt(p);
    clearStrBuffer(cmdBuffer);

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
      break;
    }
    case Call:
    case OCall: {
      p->traceDepth = 1;
      p->tracing = False;
      break;
    }
    case TCall:
    case TOCall: {
      p->traceDepth = 0;
      p->tracing = False;
      break;
    }

    default:
      p->traceDepth = 0;
      break;
  }
  return stepOver;
}

static DebugWaitFor dbgQuit(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  return quitDbg;
}

static DebugWaitFor dbgTrace(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  p->tracing = True;
  p->traceCount = cmdCount(line, 0);

  resetDeflt("n");
  if (p->traceCount != 0)
    return stepInto;
  else
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
      break;
    }
    case Call:
    case OCall: {
      p->traceDepth = 2;
      break;
    }
    case TCall:
    case TOCall: {
      p->traceDepth = 1;
      break;
    }
    case LdG: {
      stackPo stk = p->stk;
      insPo pc = currFrame(stk)->pc;
      int32 glbNo = collect32(pc + 1);
      globalPo glb = findGlobalVar(glbNo);

      if (!glbIsSet(glb))
        p->traceDepth = 2;

      break;
    }

    default:
      p->traceDepth = 1;
      break;
  }
  return stepOver;
}

static DebugWaitFor dbgSetDepth(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  displayDepth = cmdCount(line, 0);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowRegisters(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  showRegisters(p, p->heap);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowCall(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  stackPo stk = p->stk;
  showStackCall(debugOutChnnl, stk, stk->fp, 0);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowLocal(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  integer lclNo = cmdCount(line, 0);
  stackPo stk = p->stk;
  framePo fp = stk->fp;
  methodPo mtd = fp->prog;

  if (lclNo == 0)
    showAllLocals(debugOutChnnl, stk, fp);
  else if (lclNo > 0 && lclNo <= lclCount(mtd))
    showLcl(debugOutChnnl, stk, cmdCount(line, 0));
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
      default:
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
  stackPo stk = p->stk;
  framePo fp = currFrame(stk);
  ptrPo limit = stackLcl(stk, fp, lclCount(fp->prog));

  if (line[0] == '\n') {
    ptrPo sp = stk->sp;

    for (integer ix = 0; sp < limit; ix++, sp++) {
      outMsg(debugOutChnnl, "SP[%d]=%,*T\n", ix, displayDepth, *sp);
    }
  } else {
    integer count = cmdCount(line, 1);
    limit -= count;
    ptrPo sp = stk->sp;

    for (integer ix = 0; sp < limit; ix++, sp++) {
      outMsg(debugOutChnnl, "SP[%d]=%,*T\n", ix, displayDepth, *sp);
    }
  }

  resetDeflt("n");
  return moreDebug;
}

void showStackCall(ioPo out, stackPo stk, framePo fp, integer frameNo) {
  methodPo mtd = fp->prog;
  insPo pc = fp->pc;
  integer pcOffset = (integer) (pc - mtd->code);

  termPo locn = findPcLocation(mtd, pcOffset);
  if (locn != Null)
    outMsg(out, "[%d] %#L: %T(", frameNo, locn, mtd);
  else
    outMsg(out, "[%d] (unknown loc): %T[%d](", frameNo, mtd, pcOffset);

  integer count = argCount(mtd);
  char *sep = "";
  for (integer ix = 0; ix < count; ix++) {
    outMsg(out, "%s%,*T", sep, displayDepth, *stackArg(stk, fp, ix));
    sep = ", ";
  }
  outMsg(out, ")\n%_");
}

void showStackEntry(ioPo out, stackPo stk, framePo fp, integer frameNo) {
  showStackCall(out, stk, fp, frameNo);
  showAllLocals(out, stk, fp);
}

static DebugWaitFor dbgStackTrace(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  stackTrace(p, debugOutChnnl, p->stk);

  resetDeflt("n");
  return moreDebug;
}

void stackTrace(processPo p, ioPo out, stackPo stk) {
  heapPo h = processHeap(p);

  outMsg(out, "Stack trace for process %d\n", p->processNo);
#ifdef TRACEEXEC
  if (debugDebugging) {
    heapSummary(out, h);
  }
#endif

  do {
    switch (stackState(stk)) {
      case suspended:
        outMsg(out, RED_ESC_ON"Suspended"RED_ESC_OFF);
        break;
      case attached:
        outMsg(out, GREEN_ESC_ON"Attached"GREEN_ESC_OFF);
        break;
      case detached:
        outMsg(out, RED_ESC_ON"Detached"RED_ESC_OFF);
        break;
      case root:
        outMsg(out, YELLOW_ESC_ON"Root"YELLOW_ESC_OFF);
        break;
    }
    if (stackPrompt(stk) != Null) {
      outMsg(out, " prompt %T\n", stackPrompt(stk));
    } else
      outMsg(out, "\n");

#ifdef TRACEEXEC
    if (debugDebugging) {
      stackSummary(out, stk);
    }
#endif

    ptrPo sp = stk->sp;
    framePo fp = stk->fp;
    for (integer fx = 0; sp < stackLimit(stk); fx++) {
      showStackEntry(out, stk, fp, fx);
      sp = (ptrPo) (fp + 1);
      fp = fp->fp;
    }

    stk = stk->attachment;
  } while (stk != Null);

  flushFile(out);
}

static DebugWaitFor dbgShowCode(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  stackPo stk = p->stk;
  framePo f = currFrame(stk);
  methodPo mtd = f->prog;
  insPo pc = f->pc;
  integer remaining = insCount(mtd) - (pc - entryPoint(mtd));

  integer count = cmdCount(line, remaining);
  insPo last = entryPoint(mtd) + insCount(mtd);

  for (integer ix = 0; ix < count && pc < last; ix++) {
    pc = disass(debugOutChnnl, Null, mtd, pc);
    outStr(debugOutChnnl, "\n");
  }

  flushFile(debugOutChnnl);
  resetDeflt("n");

  return moreDebug;
}

void showMethodCode(ioPo out, char *name, methodPo mtd) {
  insPo pc = entryPoint(mtd);
  insPo last = entryPoint(mtd) + insCount(mtd);

  outMsg(out, "code for %s\n", name);

  while (pc < last) {
    pc = disass(out, NULL, mtd, pc);
    outMsg(out, "\n");
  }
  flushOut();
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

static DebugWaitFor dbgVerifyProcess(char *line, processPo p, termPo loc, insWord ins, void *cl) {
  lineDebugging = True;
  insDebugging = False;
  resetDeflt("n");
  verifyProc(p, processHeap(p));
  return moreDebug;
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
  stackPo stk = p->stk;

  integer frameNo = 0;
  framePo limit = (framePo) stackLimit(stk);

  while (frameNo < count && stk->fp < limit) {
    stk->sp = ((ptrPo) (stk->fp + 1));
    stk->fp = stk->fp->fp;
    frameNo++;
  }

  outMsg(debugOutChnnl, "Dropped %d frames\n%_", frameNo);
  resetDeflt("n");
  return moreDebug;
}

static logical shouldWeStopIns(processPo p, stackPo stk, insWord ins) {
  if (focus == NULL || focus == p) {
    framePo f = currFrame(stk);
#ifdef TRACE_DBG
    if (debugDebugging) {
      outMsg(logFile, "debug: traceDepth=%d, traceCount=%d, tracing=%s, ins: ", p->traceDepth, p->traceCount,
             (p->tracing ? "yes" : "no"));
      disass(logFile, stk, f->prog, f->pc);
      outMsg(logFile, "\n%_");
    }
#endif
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
      case LdG: {
        int32 glbNo = collect32(f->pc + 1);
        globalPo glb = findGlobalVar(glbNo);

        switch (p->waitFor) {
          case stepOver: {
            if (!glbIsSet(glb))
              p->traceDepth++;
          }
          default:;
        }
        return False;
      }
      case TCall:
      case TOCall:
        switch (p->waitFor) {
          case stepOver:
            return (logical) (p->traceDepth == 0 && p->traceCount == 0);
          default:
            return False;
        }
      case dLine: {
        termPo loc = findPcLocation(f->prog, insOffset(f->prog, f->pc));

        breakPointPo bp = lineBreakPointHit(C_NORMAL(loc));
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
      {.c = 'C', .cmd=dbgShowCall, .usage="C show current call"},
      {.c = 's', .cmd=dbgShowStack, .usage="s show stack"},
      {.c = 'S', .cmd=dbgStackTrace, .usage="S show entire stack"},
      {.c = 'D', .cmd=dbgDropFrame, .usage="D <count> drop stack frame(s)"},
      {.c = 'g', .cmd=dbgShowGlobal, .usage="g <var> show global var"},
      {.c = 'i', .cmd=dbgShowCode, .usage="i show instructions"},
      {.c = 'd', .cmd=dbgSetDepth, .usage="d <dpth> set display depth"},
      {.c = '+', .cmd=dbgAddBreakPoint, .usage="+ add break point"},
      {.c = '-', .cmd=dbgClearBreakPoint, .usage="- clear break point"},
      {.c = 'B', .cmd=dbgShowBreakPoints, .usage="show all break points"},
      {.c = 'y', .cmd=dbgSymbolDebug, .usage="y turn on symbolic mode"},
      {.c = 'v', .cmd=dbgVerifyProcess, .usage="v verify process"}},
    .count = 20,
    .deflt = Null
  };

  stackPo stk = p->stk;
  framePo frame = currFrame(stk);

  logical stopping = shouldWeStopIns(p, stk, ins);
  if (p->tracing || stopping) {
    outMsg(debugOutChnnl, "[(%d)%d]: ", stackNo(stk), pcCount);
    disass(debugOutChnnl, stk, frame->prog, frame->pc);

    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0)
          p->waitFor = cmder(&opts, p, frame->prog, Null, ins);
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

void showLn(ioPo out, stackPo stk, termPo ln) {
  if (showColors)
    outMsg(out, BLUE_ESC_ON"line:"BLUE_ESC_OFF" %#L%_", ln);
  else
    outMsg(out, "line: %#L%_", ln);
}

retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt) {
  termPo ln = (termPo) data;

  if (isNormalPo(ln)) {
    normalPo line = C_NORMAL(ln);
    char pkgNm[MAX_SYMB_LEN];
    copyChars2Buff(C_CHARS(nthArg(line, 0)), pkgNm, NumberOf(pkgNm));

    if (alt && showPkgFile) {
      char srcName[MAXFILELEN];
      packagePo pkg = loadedPackage(pkgNm);
      retCode ret = manifestResource(pkg, "source", srcName, NumberOf(srcName));
      if (ret == Ok)
        return outMsg(f, "%s(%T:%T@%T,%T)%_", srcName, nthArg(line, 1), nthArg(line, 2), nthArg(line, 3),
                      nthArg(line, 4));
    }
    return outMsg(f, "%s:%T:%T(%T)", pkgNm, nthArg(line, 1), nthArg(line, 2), nthArg(line, 4));
  } else
    return outMsg(f, "%,*T", displayDepth, ln);
}

static retCode shArgs(ioPo out, integer depth, stackPo stk, integer from, framePo fp) {
  char *sep = "";
  tryRet(outStr(out, "("));
  for (integer ix = from; ix < codeArity(fp->prog); ix++) {
    tryRet(outMsg(out, "%s%#,*T", sep, depth, *stackArg(stk, fp, ix)));
    sep = ", ";
  }
  return outMsg(out, ")");
}

static retCode shCall(ioPo out, char *msg, termPo locn, methodPo mtd, stackPo stk) {
  if (locn != Null) {
    tryRet(outMsg(out, "%s %#L %#.16T", msg, locn, mtd));
  } else
    tryRet(outMsg(out, "%s %#.16T", msg, mtd));

  return shArgs(out, displayDepth, stk, 0, stk->fp);
}

void showCall(ioPo out, stackPo stk, termPo call) {
  framePo f = currFrame(stk);

  termPo locn = findPcLocation(f->prog, insOffset(f->prog, f->pc));
  if (showColors)
    shCall(out, GREEN_ESC_ON"call:"GREEN_ESC_OFF, locn, labelCode(C_LBL(call)), stk);
  else
    shCall(out, "call:", locn, labelCode(C_LBL(call)), stk);
}

void showTail(ioPo out, stackPo stk, termPo call) {
  framePo f = currFrame(stk);
  termPo locn = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (showColors)
    shCall(out, GREEN_ESC_ON"tail:"GREEN_ESC_OFF, locn, labelCode(C_LBL(call)), stk);
  else
    shCall(out, "tail:", locn, labelCode(C_LBL(call)), stk);

}

static retCode shOCall(ioPo out, char *msg, stackPo stk, termPo call) {
  framePo f = currFrame(stk);
  termPo locn = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (locn != Null) {
    tryRet(outMsg(out, "%s %#L ", msg, locn));
  } else
    tryRet(outMsg(out, "%s ", msg));

  tryRet(outMsg(out, "%#,*T", displayDepth, *stackArg(stk, stk->fp, 0)));
  tryRet(outMsg(out, "•%#.16T", f->prog));
  return shArgs(out, displayDepth, stk, 1, stk->fp);
}

void showOCall(ioPo out, stackPo stk, termPo call) {
  if (showColors)
    shOCall(out, GREEN_ESC_ON"ocall:"GREEN_ESC_OFF, stk, call);
  else
    shOCall(out, "ocall:", stk, call);
}

void showOTail(ioPo out, stackPo stk, termPo call) {
  if (showColors)
    shOCall(out, GREEN_ESC_ON"otail:"GREEN_ESC_OFF, stk, call);
  else
    shOCall(out, "otail:", stk, call);

}

void showRet(ioPo out, stackPo stk, termPo val) {
  framePo f = currFrame(stk);
  termPo locn = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (locn != Null) {
    if (showColors)
      outMsg(out, RED_ESC_ON"return:"RED_ESC_OFF" %#L %T->%#,*T", locn, f->prog, displayDepth, val);
    else
      outMsg(out, "return: %#L %T->%#,*T", locn, f->prog, displayDepth, val);
  } else
    outMsg(out, "return: %T->%#,*T", f->prog, displayDepth, val);
}

typedef void (*showCmd)(ioPo out, stackPo stk, termPo trm);

termPo getLbl(termPo lbl, int32 arity) {
  labelPo oLbl = isNormalPo(lbl) ? termLbl(C_NORMAL(lbl)) : C_LBL(lbl);
  return (termPo) declareLbl(oLbl->name, arity, -1);
}

static DebugWaitFor lnDebug(processPo p, insWord ins, termPo ln, showCmd show);

DebugWaitFor callDebug(processPo p, termPo call) {
  return lnDebug(p, Call, Null, showCall);
}

DebugWaitFor ocallDebug(processPo p, termPo call) {
  return lnDebug(p, OCall, call, showOCall);
}

DebugWaitFor tailDebug(processPo p, termPo call) {
  return lnDebug(p, TCall, call, showTail);
}

DebugWaitFor otailDebug(processPo p, termPo call) {
  return lnDebug(p, TOCall, call, showOTail);
}

DebugWaitFor retDebug(processPo p, termPo val) {
  return lnDebug(p, Ret, val, showRet);
}

DebugWaitFor lineDebug(processPo p, termPo line) {
  return lnDebug(p, dLine, line, showLn);
}

DebugWaitFor enterDebug(processPo p) {
  stackPo stk = p->stk;
  framePo f = currFrame(stk);
  insPo pc = f->pc;
  insWord ins = *pc++;
  switch (ins) {
    case Call:
      return callDebug(p, getMtdLit(f->prog, collect32(pc)));
    case TCall:
      return tailDebug(p, getMtdLit(f->prog, collect32(pc)));
    case OCall: {
      int32 arity = collect32(pc);
      termPo callee = getLbl(*stackArg(stk, stk->fp, 0), arity);
      return ocallDebug(p, callee);
    }
    case TOCall: {
      int32 arity = collect32(pc);
      termPo callee = getLbl(*stackArg(stk, stk->fp, 0), arity);
      return otailDebug(p, callee);
    }
    case Ret:
      return retDebug(p, *stackArg(stk, stk->fp, 0));
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
    {.c = 'C', .cmd=dbgShowCall, .usage="C show current call"},
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
    .count = 19,
    .deflt = Null
  };

  stackPo stk = p->stk;
  logical stopping = shouldWeStop(p, stk, ins, ln);

#ifdef TRACE_DBG
  if (debugDebugging) {
    logMsg(logFile, "traceCount=%d, traceDepth=%d, stopping=%s, tracing=%s", p->traceCount, p->traceDepth,
           (stopping ? "yes" : "no"), (p->tracing ? "yes" : "no"));
  }
#endif
  if (p->tracing || stopping) {
    if (ln != Null)
      show(debugOutChnnl, stk, ln);
    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0)
          p->waitFor = cmder(&opts, p, currFrame(stk)->prog, ln, ins);
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

void stackSummary(ioPo out, stackPo stk) {
  integer freeCount = stk->sp - stk->stkMem;
  integer used = stk->sze - freeCount;
  outMsg(out, "used: %l, free:%5.2g%%", used, ((double) freeCount) / (double) stk->sze);
}

retCode showArg(ioPo out, stackPo stk, integer arg) {
  if (stk != Null) {
    return outMsg(out, " A[%d] = %,*T", arg, displayDepth, *stackArg(stk, stk->fp, arg));
  } else
    return outMsg(out, " A[%d]", arg);
}

void showAllLocals(ioPo out, stackPo stk, framePo fp) {
  methodPo mtd = fp->prog;
  for (integer vx = 1; vx <= lclCount(mtd); vx++) {
    char vName[MAX_SYMB_LEN];
    if (localVName(mtd, fp->pc, vx, vName, NumberOf(vName)) == Ok) {
      ptrPo var = stackLcl(stk, fp, vx);
      if (*var != Null && *var != voidEnum)
        outMsg(out, "  %s(%d) = %#,*T\n", vName, vx, displayDepth, *var);
      else
        outMsg(out, "  %s(%d) (unset)\n", vName, vx);
    }
  }
}

retCode showLcl(ioPo out, stackPo stk, integer vr) {
  if (stk != Null)
    return outMsg(out, " l[%d] = %,*T", vr, displayDepth, *stackLcl(stk, stk->fp, vr));
  else
    return outMsg(out, " l[%d]", vr);
}

retCode showGlb(ioPo out, globalPo glb) {
  if (glb != Null) {
    if (glbIsSet(glb))
      return outMsg(out, " %s=%,*T", globalVarName(glb), displayDepth, getGlobal(glb));
    else
      return outMsg(out, " %s (undef)", globalVarName(glb));
  } else
    return outMsg(out, " unknown global");
}

retCode showTos(ioPo out, stackPo stk) {
  if (stk != Null)
    return outMsg(out, " <tos> = %,*T", displayDepth, *stk->sp);
  else
    return outMsg(out, " <tos>");
}

static void showTopOfStack(ioPo out, stackPo stk, integer cnt) {
  if (stk != Null) {
    char *sep = "";
    ptrPo sp = stk->sp;
    outMsg(out, " %s%,*T(", sep, displayDepth, *sp++);

    for (integer ix = 1; ix < cnt; ix++) {
      outMsg(out, "%s%,*T", sep, displayDepth, *sp++);
      sep = ", ";
    }

    outMsg(out, ")%_");
  } else
    outStr(out, " <tos>");
}

static void showPcOffset(ioPo out, insPo base, insPo *pc) {
  uint32 hi32 = (uint32) (*pc)[0];
  uint32 lo32 = (uint32) (*pc)[1];
  (*pc) += 2;
  uint32 delta = (hi32 << 16u) | lo32;

  outMsg(out, " PC[%d(%+d)]", (*pc - base) + delta, delta);
}

static void showEscCall(ioPo out, insPo base, insPo *pc) {
  uint32 hi32 = (uint32) (*pc)[0];
  uint32 lo32 = (uint32) (*pc)[1];
  (*pc) += 2;
  uint32 escNo = (hi32 << 16u) | lo32;
  escapePo esc = getEscape(escNo);
  outMsg(out, " %s/%d", esc->name, esc->arity);
}

insPo disass(ioPo out, stackPo stk, methodPo mtd, insPo pc) {
  int32 hi32, lo32;

  insPo entry = entryPoint(mtd);
  integer offset = (integer) (pc - entry);

  normalPo lits = codeLits(mtd);
  if (lits != Null)
    outMsg(out, "%,*T [%d] ", displayDepth, nthArg(codeLits(mtd), 0), offset);
  else
    outMsg(out, "\?\? [%d] ", offset);

  switch (*pc++) {
#undef instruction

#define show_nOp
#define show_tOs showTos(out,stk)
#define show_art showTopOfStack(out,stk,collectI32(pc))
#define show_i32 outMsg(out," #%d",collectI32(pc))
#define show_lBs outMsg(out," #%d",collectI32(pc))
#define show_arg showArg(out,stk,collectI32(pc))
#define show_lcl showLcl(out,stk,collectI32(pc))
#define show_lcs outMsg(out," l[%d]",collectI32(pc))
#define show_off showPcOffset(out,entry,&pc)
#define show_cDe showPcOffset(out,entry,&pc)
#define show_lVl showPcOffset(out,entry,&pc)
#define show_sym showConstant(out,mtd,collectI32(pc))
#define show_Es showEscCall(out, entry, &pc)
#define show_lit showConstant(out,mtd,collectI32(pc))
#define show_lne showConstant(out,mtd,collectI32(pc))
#define show_glb showGlb(out, findGlobalVar(collectI32(pc)))
#define show_tPe showLTipe(out,mtd,collectI32(pc))

#define instruction(Op, A1, A2, Dl, Cmt)    \
    case Op:                                \
      outMsg(out," %s",#Op);                \
      show_##A1;                            \
      show_##A2;                            \
      return pc;

#include "instructions.h"

    default:
      return pc;
  }
}

void showRegisters(processPo p, heapPo h) {
  stackPo stk = p->stk;
  showStackEntry(debugOutChnnl, stk, stk->fp, 0);

#ifdef TRACEEXEC
  if (debugDebugging) {
    stackSummary(debugOutChnnl, stk);
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
    normalPo vr = C_NORMAL(nthArg(locals, ix));
    integer from = integerVal(nthArg(vr, 1));
    integer to = integerVal(nthArg(vr, 2));

    if (from <= pcOffset && to > pcOffset && integerVal(nthArg(vr, 3)) == vNo) {
      copyChars2Buff(C_CHARS(nthArg(vr, 0)), buffer, bufLen);

      if (uniIsLitPrefix(buffer, anonPrefix))
        uniCpy(buffer, bufLen, "l");
      return Ok;
    }
  }
  return Fail;
}

void dumpStats() {
  logMsg(debugOutChnnl, "%ld instructions executed\n", pcCount);
  dumpEscapes(debugOutChnnl);
}

