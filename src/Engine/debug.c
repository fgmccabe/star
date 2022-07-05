// Incremental instruction debugger

#include "engineP.h"
#include <stdlib.h>
#include <globals.h>
#include <strings.h>
#include <ioTcp.h>
#include <manifest.h>
#include <termcap.h>

#include "debugP.h"
#include "arith.h"
#include "editline.h"

integer pcCount = 0;
static integer lineCount = 0;

static void showAbort(ioPo out, taskPo stk, termPo reason);
static void showCall(ioPo out, taskPo stk, termPo call);
static void showTail(ioPo out, taskPo stk, termPo call);
static void showOCall(ioPo out, taskPo stk, termPo call);
static void showOTail(ioPo out, taskPo stk, termPo call);
static void showRet(ioPo out, taskPo stk, termPo val);
static void showRetX(ioPo out, taskPo stk, termPo val);
static void showAssign(ioPo out, taskPo stk, termPo vl);
static void showResume(ioPo out, taskPo stk, termPo cont);
static void showTResume(ioPo out, taskPo stk, termPo cont);
static void showGlobal(ioPo out, taskPo stk, termPo global);

static void showRegisters(processPo p, heapPo h);
static void showAllLocals(ioPo out, taskPo stk, framePo fp);
static void showTos(ioPo out, taskPo stk, termPo tos);
static retCode showLcl(ioPo out, taskPo stk, integer vr);
static retCode showArg(ioPo out, taskPo stk, integer arg);
static void showStackCall(ioPo out, taskPo stk, framePo fp, integer frameNo);
static retCode localVName(methodPo mtd, insPo pc, integer vNo, char *buffer, integer bufLen);
static void stackSummary(ioPo out, taskPo stk);

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

ReturnStatus g__ins_debug(processPo p, heapPo h) {
  insDebugging = tracing = True;
  currentProcess->waitFor = stepInto;
  currentProcess->tracing = True;
  currentProcess->traceCount = 0;
  currentProcess->waterMark = p->stk->fp;

  return rtnStatus(h, Ok, "_ins_debug");
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

// Figuring out if we should stop is surprisingly complicated
static logical shouldWeStop(processPo p, termPo arg) {
  if (focus == NULL || focus == p) {
    taskPo stk = p->stk;
    framePo frame = currFrame(stk);

    if (debugDebugging) {
      outMsg(logFile, "debug: waterMark=0x%x, fp=0x%x, traceCount=%d, tracing=%s, ins: ", p->waterMark, p->stk->fp,
             p->traceCount, (p->tracing ? "yes" : "no"));
      disass(logFile, stk, frame->prog, frame->pc);
      outMsg(logFile, "\n%_");
    }

    switch (*p->stk->fp->pc) {
      case Abort:
        return True;
      case Nop:
      case Ret:
      case RetX:
      case RtG: {
        switch (p->waitFor) {
          case stepOver:
            return (logical) (p->waterMark == p->stk->fp->fp && p->traceCount == 0);
          case stepInto:
            if (p->traceCount > 0)
              p->traceCount--;
            return (logical) (p->traceCount == 0);
          default:
            return False;
        }
      }
      case Call:
      case OCall:
      case TCall:
      case TOCall: {
        if (breakPointSet(C_LBL(arg))) {
          p->waitFor = stepInto;
          p->tracing = True;
          p->waterMark = Null;
          return True;
        } else
          switch (p->waitFor) {
            case stepInto:
              if (p->traceCount > 0)
                p->traceCount--;
              return (logical) (p->traceCount == 0);
            case stepOver:
              return (logical) (p->traceCount == 0 && p->waterMark == p->stk->fp);
            default:
              return False;
          }
      }

      case LdG: {
        if (!glbIsSet(C_GLOB(arg))) {
          switch (p->waitFor) {
            case stepOver:
              return (logical) (p->waterMark == p->stk->fp && p->traceCount == 0);
            case stepInto:
              if (p->traceCount > 0)
                p->traceCount--;
              return (logical) (p->traceCount == 0);
            default:
              return False;
          }
        } else
          return False;
      }
      default: {
        switch (p->waitFor) {
          case stepInto:
            if (p->traceCount > 0)
              p->traceCount--;
            return (logical) (p->traceCount == 0);
          case stepOver:
            return (logical) (p->traceCount == 0 && p->waterMark == p->stk->fp);
          default:
            return False;
        }
      }
    }
  } else
    return False;
}

static char *defltLine(char *src, integer srcLen, integer *actLen) {
  static char defltLn[MAXLINE] = {'n'};

  if (!uniIsTrivial(src, srcLen)) {
    uniTrim(src, srcLen, " \n", " \n", defltLn, NumberOf(defltLn));
    *actLen = uniStrLen(defltLn);
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

static DebugWaitFor cmder(debugOptPo opts, processPo p, methodPo mtd, termPo loc) {
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
            return opts->opts[ix].cmd(&cmdLine[nxt], p, loc, opts->opts[ix].cl);
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

static DebugWaitFor dbgSingle(char *line, processPo p, termPo loc, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->waterMark = Null;
  p->tracing = (logical) (p->traceCount == 0);
  return stepInto;
}

static DebugWaitFor dbgOver(char *line, processPo p, termPo loc, void *cl) {
  p->traceCount = cmdCount(line, 0);

  switch (*p->stk->fp->pc) {
    case Ret:
    case RetX:
    case RtG: {
      p->waterMark = p->stk->fp->fp;
      break;
    }

    default:
      p->waterMark = p->stk->fp;
      break;
  }
  p->tracing = False;

  resetDeflt("n");
  return stepOver;
}

static DebugWaitFor dbgQuit(char *line, processPo p, termPo loc, void *cl) {
  return quitDbg;
}

static DebugWaitFor dbgTrace(char *line, processPo p, termPo loc, void *cl) {
  p->tracing = True;
  p->traceCount = cmdCount(line, 0);

  resetDeflt("n");
  if (p->traceCount != 0)
    return stepInto;
  else
    return nextBreak;
}

static DebugWaitFor dbgCont(char *line, processPo p, termPo loc, void *cl) {
  p->tracing = False;

  resetDeflt("n");
  return nextBreak;
}

static DebugWaitFor dbgUntilRet(char *line, processPo p, termPo loc, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->tracing = False;
  resetDeflt("n");

  switch (*p->stk->fp->pc) {
    case Ret:
    case RetX:
    case RtG: {
      p->waterMark = p->stk->fp->fp;
      break;
    }
    case Call:
    case OCall: {
      p->waterMark = p->stk->fp;
      break;
    }
    case TCall:
    case TOCall: {
      p->waterMark = p->stk->fp->fp;
      break;
    }

    default:
      p->waterMark = p->stk->fp;
      break;
  }
  return stepOver;
}

static DebugWaitFor dbgSetDepth(char *line, processPo p, termPo loc, void *cl) {
  integer depth = cmdCount(line, 0);
  if (depth > 0)
    displayDepth = cmdCount(line, 0);
  else
    outMsg(debugOutChnnl, "display depth %ld\n%_", displayDepth);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowRegisters(char *line, processPo p, termPo loc, void *cl) {
  showRegisters(p, p->heap);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowCall(char *line, processPo p, termPo loc, void *cl) {
  taskPo stk = p->stk;
  showStackCall(debugOutChnnl, stk, stk->fp, 0);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowLocal(char *line, processPo p, termPo loc, void *cl) {
  integer lclNo = cmdCount(line, 0);
  taskPo stk = p->stk;
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

static DebugWaitFor dbgShowGlobal(char *line, processPo p, termPo loc, void *cl) {
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

static DebugWaitFor dbgShowStack(char *line, processPo p, termPo loc, void *cl) {
  taskPo stk = p->stk;
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

void showStackCall(ioPo out, taskPo stk, framePo fp, integer frameNo) {
  methodPo mtd = fp->prog;
  if (normalCode(mtd)) {
    insPo pc = fp->pc;
    integer pcOffset = (integer) (pc - mtd->code);

    termPo locn = findPcLocation(mtd, pcOffset);
    if (locn != Null)
      outMsg(out, "[%d] %#L: %#T(", frameNo, locn, mtd);
    else
      outMsg(out, "[%d] (unknown loc): %#T[%d](", frameNo, mtd, pcOffset);

    integer count = argCount(mtd);
    char *sep = "";
    for (integer ix = 0; ix < count; ix++) {
      outMsg(out, "%s%#,*T", sep, displayDepth, *stackArg(stk, fp, ix));
      sep = ", ";
    }
    outMsg(out, ")\n%_");
  }
}

void showStackEntry(ioPo out, taskPo stk, framePo fp, integer frameNo, logical showLocals) {
  showStackCall(out, stk, fp, frameNo);
  if (showLocals)
    showAllLocals(out, stk, fp);
}

static DebugWaitFor dbgStackTrace(char *line, processPo p, termPo loc, void *cl) {
  stackTrace(p, debugOutChnnl, p->stk, True);

  resetDeflt("n");
  return moreDebug;
}

void stackTrace(processPo p, ioPo out, taskPo stk, logical showLocals) {
  outMsg(out, "Stack trace for process %d\n", p->processNo);

  do {
    switch (taskState(stk)) {
      case suspended:
        outMsg(out, RED_ESC_ON"Suspended"RED_ESC_OFF);
        break;
      case active:
        outMsg(out, GREEN_ESC_ON"Active"GREEN_ESC_OFF);
        break;
      case moribund:
        outMsg(out, RED_ESC_ON"Moribund"RED_ESC_OFF);
        break;
    }

    outMsg(out, "\n");

#ifdef TRACEEXEC
    if (debugDebugging) {
      stackSummary(out, stk);
    }
#endif

    ptrPo sp = stk->sp;
    framePo fp = stk->fp;
    integer frameNo = 0;
    for (; sp < stackLimit(stk); frameNo++) {
      showStackEntry(out, stk, fp, frameNo, showLocals);
      sp = (ptrPo) (fp + 1);
      fp = fp->fp;
    }

    stk = stk->attachment;
  } while (stk != Null);

  flushFile(out);
}

static DebugWaitFor dbgShowCode(char *line, processPo p, termPo loc, void *cl) {
  taskPo stk = p->stk;
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

void showMethodCode(ioPo out, char *msg, char *name, methodPo mtd) {
  insPo pc = entryPoint(mtd);
  insPo last = entryPoint(mtd) + insCount(mtd);

  outMsg(out, msg, name);

  while (pc < last) {
    pc = disass(out, NULL, mtd, pc);
    outMsg(out, "\n");
  }
  flushOut();
}

static DebugWaitFor dbgDebug(char *line, processPo p, termPo loc, void *cl) {
  debugDebugging = !debugDebugging;

  logMsg(stdErr, "debug debugging %s\n", (debugDebugging ? "enanbled" : "disabled"));
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgInsDebug(char *line, processPo p, termPo loc, void *cl) {
  lineDebugging = False;
  insDebugging = True;
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgSymbolDebug(char *line, processPo p, termPo loc, void *cl) {
  lineDebugging = True;
  insDebugging = False;
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgVerifyProcess(char *line, processPo p, termPo loc, void *cl) {
  lineDebugging = True;
  insDebugging = False;
  resetDeflt("n");
  verifyProc(p, processHeap(p));
  return moreDebug;
}

static DebugWaitFor dbgAddBreakPoint(char *line, processPo p, termPo loc, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok) {
    integer count = addBreakPoints(&bp);
    outMsg(debugOutChnnl, "%d break points set\n%_", count);
  } else
    outMsg(debugOutChnnl, "could not set spy points on %s\n%_", line);
  return moreDebug;
}

DebugWaitFor dbgClearBreakPoint(char *line, processPo p, termPo loc, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok) {
    integer count = clearBreakPoints(&bp);
    outMsg(debugOutChnnl, "%d break points cleared\n%_", count);
  } else
    outMsg(debugOutChnnl, "could not clear spy points on %s\n%_", line);
  return moreDebug;
}

DebugWaitFor dbgShowBreakPoints(char *line, processPo p, termPo loc, void *cl) {
  retCode ret = showAllBreakPoints(debugOutChnnl);

  if (ret != Ok)
    outMsg(debugOutChnnl, "Could not show break points\n%_");
  return moreDebug;
}

static DebugWaitFor dbgDropFrame(char *line, processPo p, termPo loc, void *cl) {
  integer count = cmdCount(line, 0);
  taskPo stk = p->stk;

  integer frameNo = 0;
  framePo limit = (framePo) stackLimit(stk);

  while (frameNo < count && stk->fp < limit) {
    stk->sp = ((ptrPo) (stk->fp + 1));
    stk->fp = stk->fp->fp;
    frameNo++;
  }

  stk->fp->pc = entryPoint(stk->fp->prog);

  outMsg(debugOutChnnl, "Dropped %d frames\n%_", frameNo);
  showStackCall(debugOutChnnl, stk, stk->fp, 0);

  resetDeflt("n");
  return moreDebug;
}

static logical shouldWeStopIns(processPo p) {
  taskPo stk = p->stk;
  if (focus == NULL || focus == p) {
    framePo f = currFrame(stk);
#ifdef TRACE_DBG
    if (debugDebugging) {
      outMsg(logFile, "debug: waterMark=0x%x, fp=0x%x, traceCount=%d, tracing=%s, ins: ", p->waterMark, p->stk->fp,
             p->traceCount, (p->tracing ? "yes" : "no"));
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

    switch (*f->pc) {
      case Ret:
      case RetX:
      case RtG: {
        switch (p->waitFor) {
          case stepOver:
            return (logical) (p->stk->fp->fp && p->traceCount == 0);
          default:
            return False;
        }
      }
      case Call:
      case OCall: {
        switch (p->waitFor) {
          case stepOver:
            return (logical) (p->stk->fp == p->waterMark);
          default:
            return False;
        }
      }
      case LdG: {
        int32 glbNo = collect32(f->pc);
        globalPo glb = findGlobalVar(glbNo);

        if (!glbIsSet(glb)) {
          if (p->waitFor == stepOver)
            return (logical) (p->waterMark == p->stk->fp && p->traceCount == 0);
          else
            return False;
        } else
          return False;
      }
      case TCall:
      case TOCall:
        switch (p->waitFor) {
          case stepOver:
            return (logical) (p->stk->fp == p->waterMark && p->traceCount == 0);
          default:
            return False;
        }

      default:
        return False;
    }
  } else
    return False;
}

DebugWaitFor insDebug(processPo p) {
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

  logical stopping = shouldWeStopIns(p);
  if (p->tracing || stopping) {
    taskPo stk = p->stk;
    outMsg(debugOutChnnl, "[(%d)%d]: ", stackNo(stk), pcCount);
    disass(debugOutChnnl, stk, stk->fp->prog, stk->fp->pc);

    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0)
          p->waitFor = cmder(&opts, p, stk->fp->prog, Null);
        else {
          outStr(debugOutChnnl, "\n");
        }

        flushFile(debugOutChnnl);
        taskSanityCheck(p->stk);

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

retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt) {
  termPo ln = (termPo) data;

  if (isNormalPo(ln)) {
    normalPo line = C_NORMAL(ln);
    char pkgNm[MAX_SYMB_LEN];
    copyChars2Buff(C_STR(nthArg(line, 0)), pkgNm, NumberOf(pkgNm));

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

static retCode shArgs(ioPo out, integer depth, ptrPo sp, integer from, integer arity) {
  char *sep = "";
  tryRet(outStr(out, "("));
  for (integer ix = from; ix < arity; ix++) {
    tryRet(outMsg(out, "%s%#,*T", sep, depth, sp[ix]));
    sep = ", ";
  }
  return outMsg(out, ")");
}

static retCode shCall(ioPo out, char *msg, termPo locn, methodPo mtd, taskPo stk) {
  if (locn != Null) {
    tryRet(outMsg(out, "%s %#L %#.16T", msg, locn, mtd));
  } else
    tryRet(outMsg(out, "%s %#.16T", msg, mtd));

  return shArgs(out, displayDepth, stk->sp, 0, codeArity(mtd));
}

void showCall(ioPo out, taskPo stk, termPo call) {
  framePo f = currFrame(stk);

  termPo locn = findPcLocation(f->prog, insOffset(f->prog, f->pc));
  if (showColors)
    shCall(out, GREEN_ESC_ON"call:"GREEN_ESC_OFF, locn, labelCode(C_LBL(call)), stk);
  else
    shCall(out, "call:", locn, labelCode(C_LBL(call)), stk);
}

void showTail(ioPo out, taskPo stk, termPo call) {
  framePo f = currFrame(stk);
  termPo locn = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (showColors)
    shCall(out, GREEN_ESC_ON"tail:"GREEN_ESC_OFF, locn, labelCode(C_LBL(call)), stk);
  else
    shCall(out, "tail:", locn, labelCode(C_LBL(call)), stk);
}

static retCode shOCall(ioPo out, char *msg, taskPo stk, termPo call) {
  framePo f = currFrame(stk);
  labelPo oLbl = C_LBL(call);
  integer arity = labelArity(oLbl);

  termPo locn = findPcLocation(stk->fp->prog, insOffset(stk->fp->prog, f->pc));
  methodPo mtd = labelCode(oLbl);       /* set up for object call */

  if (locn != Null) {
    tryRet(outMsg(out, "%s %#L ", msg, locn));
  } else
    tryRet(outMsg(out, "%s ", msg));

  tryRet(outMsg(out, "%#,*T", displayDepth, topStack(stk)));
  return shArgs(out, displayDepth, stk->sp, 1, arity);
}

void showOCall(ioPo out, taskPo stk, termPo call) {
  if (showColors)
    shOCall(out, GREEN_ESC_ON"ocall:"GREEN_ESC_OFF, stk, call);
  else
    shOCall(out, "ocall:", stk, call);
}

void showOTail(ioPo out, taskPo stk, termPo call) {
  if (showColors)
    shOCall(out, GREEN_ESC_ON"otail:"GREEN_ESC_OFF, stk, call);
  else
    shOCall(out, "otail:", stk, call);

}

void showRet(ioPo out, taskPo stk, termPo val) {
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

void showRetX(ioPo out, taskPo stk, termPo val) {
  framePo f = currFrame(stk);
  termPo locn = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (locn != Null) {
    if (showColors)
      outMsg(out, RED_ESC_ON"exception return:"RED_ESC_OFF" %#L %T->%#,*T", locn, f->prog, displayDepth, val);
    else
      outMsg(out, "exception return: %#L %T->%#,*T", locn, f->prog, displayDepth, val);
  } else
    outMsg(out, "exception return: %T->%#,*T", f->prog, displayDepth, val);
}

static void showAbort(ioPo out, taskPo stk, termPo reason) {
  framePo f = currFrame(stk);
  termPo loc = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (loc != Null) {
    if (showColors)
      outMsg(out, RED_ESC_ON"abort:"RED_ESC_OFF" %#L %T->%#,*T", loc, f->prog, displayDepth, reason);
    else
      outMsg(out, "abort: %#L %T->%#,*T", loc, f->prog, displayDepth, reason);
  } else
    outMsg(out, "abort: %T->%#,*T", f->prog, displayDepth, reason);
}

void showAssign(ioPo out, taskPo stk, termPo vl) {
  framePo f = currFrame(stk);
  termPo locn = findPcLocation(f->prog, insOffset(f->prog, f->pc));
  termPo cell = topStack(stk);
  termPo val = peekStack(stk, 1);

  if (locn != Null) {
    if (showColors)
      outMsg(out, RED_ESC_ON"assign:"RED_ESC_OFF" %#L %T->%#,*T", locn, cell, displayDepth, val);
    else
      outMsg(out, "assign: %#L %T->%#,*T", locn, cell, displayDepth, val);
  } else
    outMsg(out, "assign: %T->%#,*T", cell, displayDepth, val);
}

void showSuspend(ioPo out, taskPo stk, termPo cont) {
  framePo f = currFrame(stk);
  termPo loc = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (showColors)
    outMsg(out, CYAN_ESC_ON"suspend:"CYAN_ESC_OFF "%#L %#,*T", loc, displayDepth, cont);
  else
    outMsg(out, "suspend:", "%#L %#,*T", loc, displayDepth, cont);
}

void showResume(ioPo out, taskPo stk, termPo cont) {
  framePo f = currFrame(stk);
  termPo loc = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (showColors)
    outMsg(out, CYAN_ESC_ON"resume:"CYAN_ESC_OFF "%#L %#,*T", loc, displayDepth, cont);
  else
    outMsg(out, "resume:", "%#L %#,*T", loc, displayDepth, cont);
}

void showRetire(ioPo out, taskPo stk, termPo cont) {
  framePo f = currFrame(stk);
  termPo loc = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  if (showColors)
    outMsg(out, CYAN_ESC_ON"retire:"CYAN_ESC_OFF "%#L %#,*T", loc, displayDepth, cont);
  else
    outMsg(out, "retire:", "%#L %#,*T", loc, displayDepth, cont);
}

void showGlobal(ioPo out, taskPo stk, termPo global) {
  framePo f = currFrame(stk);
  termPo loc = findPcLocation(f->prog, insOffset(f->prog, f->pc));

  globalPo glb = C_GLOB(global);

  if (showColors)
    outMsg(out, BLUE_ESC_ON"global:"BLUE_ESC_OFF "%#L %#,*T", loc, displayDepth, global);
  else
    outMsg(out, "global: %#L %#,*T", loc, displayDepth, global);
}

typedef void (*showCmd)(ioPo out, taskPo stk, termPo trm);

termPo getLbl(termPo lbl, int32 arity) {
  labelPo oLbl = isNormalPo(lbl) ? termLbl(C_NORMAL(lbl)) : C_LBL(lbl);
  return (termPo) declareLbl(oLbl->name, arity, -1);
}

static DebugWaitFor lnDebug(processPo p, termPo arg, showCmd show);

DebugWaitFor enterDebug(processPo p) {
  taskPo stk = p->stk;
  framePo f = currFrame(stk);
  insPo pc = f->pc;
  insWord ins = *pc++;
  lineCount++;
  switch (ins) {
    case Nop: {
      return lnDebug(p, topStack(stk), showTos);
    }
    case Abort: {
      return lnDebug(p, peekStack(stk, 1), showAbort);
    }
    case Call: {
      return lnDebug(p, getMtdLit(f->prog, collect32(pc)), showCall);
    }
    case TCall: {
      return lnDebug(p, getMtdLit(f->prog, collect32(pc)), showTail);
    }
    case OCall: {
      return lnDebug(p, getLbl(topStack(stk), collect32(pc)), showOCall);
    }
    case TOCall: {
      return lnDebug(p, getLbl(topStack(stk), collect32(pc)), showOTail);
    }
    case Ret:
    case RtG: {
      return lnDebug(p, topStack(stk), showRet);
    }
    case RetX:
      return lnDebug(p, topStack(stk), showRetX);

    case Assign:
      return lnDebug(p, Null, showAssign);
    case Suspend: {
      termPo cont = topStack(stk);
      return lnDebug(p, cont, showSuspend);
    }
    case Resume: {
      termPo cont = topStack(stk);
      return lnDebug(p, cont, showResume);
    }
    case Retire: {
      termPo cont = topStack(stk);
      return lnDebug(p, cont, showRetire);
    }
    case LdG: {
      int32 glbNo = collect32(pc);
      globalPo glb = findGlobalVar(glbNo);

      return lnDebug(p, (termPo) glb, showGlobal);
    }
    default:
      return stepOver;
  }
}

DebugWaitFor lnDebug(processPo p, termPo arg, showCmd show) {
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
    {.c = 'y', .cmd=dbgInsDebug, .usage="y turn on instruction mode"},
    {.c = '&', .cmd=dbgDebug, .usage="& flip debug debugging"}},
    .count = 20,
    .deflt = Null
  };

  taskPo stk = p->stk;
  logical stopping = shouldWeStop(p, arg);

#ifdef TRACE_DBG
  if (debugDebugging) {
    logMsg(logFile, "traceCount=%d, waterMark=%x, stopping=%s, tracing=%s", p->traceCount, p->waterMark,
           (stopping ? "yes" : "no"), (p->tracing ? "yes" : "no"));
  }
#endif
  if (p->tracing || stopping) {
    show(debugOutChnnl, stk, arg);
    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0) {
          framePo f = currFrame(stk);
          termPo loc = findPcLocation(f->prog, insOffset(f->prog, f->pc));

          p->waitFor = cmder(&opts, p, currFrame(stk)->prog, loc);
        } else {
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

void stackSummary(ioPo out, taskPo stk) {
  integer freeCount = stk->sp - stk->stkMem;
  integer used = stk->sze - freeCount;
  outMsg(out, "used: %l, free:%5.2g%%", used, ((double) freeCount) / (double) stk->sze);
}

retCode showArg(ioPo out, taskPo stk, integer arg) {
  if (stk != Null) {
    return outMsg(out, " A[%d] = %,*T", arg, displayDepth, *stackArg(stk, stk->fp, arg));
  } else
    return outMsg(out, " A[%d]", arg);
}

void showAllLocals(ioPo out, taskPo stk, framePo fp) {
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

retCode showLcl(ioPo out, taskPo stk, integer vr) {
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

void showTos(ioPo out, taskPo stk, termPo _) {
  if (stk != Null)
    outMsg(out, " <tos> = %,*T", displayDepth, topStack(stk));
  else
    outMsg(out, " <tos>");
}

static void showTopOfStack(ioPo out, taskPo stk, integer cnt) {
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

insPo disass(ioPo out, taskPo stk, methodPo mtd, insPo pc) {
  int32 hi32, lo32;

  insPo entry = entryPoint(mtd);
  integer offset = (integer) (pc - entry);

  normalPo lits = codeLits(mtd);
  if (lits != Null)
    outMsg(out, "%,*T [%d] ", displayDepth, nthArg(lits, 0), offset);
  else
    outMsg(out, "\?\? [%d] ", offset);

  switch (*pc++) {
#undef instruction

#define show_nOp
#define show_tOs showTos(out,stk,Null)
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
  taskPo stk = p->stk;
  showStackEntry(debugOutChnnl, stk, stk->fp, 0, True);

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
      copyChars2Buff(C_STR(nthArg(vr, 0)), buffer, bufLen);

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
  dumpGcStats();
  dumpStackStats();
}

