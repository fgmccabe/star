// Incremental instruction debugger

#include "engineP.h"
#include <stdlib.h>
#include <globals.h>
#include <sock.h>
#include <manifest.h>

#include "debugP.h"
#include "arith.h"
#include "editline.h"
#include "ltype.h"
#include "libEscapes.h"

integer pcCount = 0;
static integer lineCount = 0;

static void showEntry(ioPo out, stackPo stk, termPo call);
static void showAbort(ioPo out, stackPo stk, termPo reason);
static void showRet(ioPo out, stackPo stk, termPo val);
static void showAssign(ioPo out, stackPo stk, termPo vl);
static void showResume(ioPo out, stackPo stk, termPo cont);
static void showRegisters(processPo p, heapPo h);
static void showAllLocals(ioPo out, stackPo stk, framePo fp);
static void showTos(ioPo out, stackPo stk, integer offset);
static retCode showLcl(ioPo out, stackPo stk, integer vr);
static retCode showArg(ioPo out, stackPo stk, integer arg);
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
logical stackVerify = False;

/* Handle suspension reasonably ... */
static void sig_suspend(int sig) {
  if (!interactive) {
    reset_stdin();    /* Reset the standard input channel */
    raise(SIGSTOP);             /* Actually suspend */
    setup_stdin();              /* Put it back */
  } else
    raise(SIGSTOP);             /* Actually suspend */
}

retCode setupDebugChannels() {
  setupSimpleHandler(SIGTSTP, sig_suspend);

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

ReturnStatus g__ins_debug(heapPo h) {
  insDebugging = tracing = True;
  currentProcess->waitFor = stepInto;
  currentProcess->tracing = True;
  currentProcess->traceCount = 0;
  currentProcess->waterMark = currentProcess->stk->fp;

  return (ReturnStatus) {.ret=Normal, .result = unitEnum};
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

__attribute__((unused)) void dC(termPo w) {
  outMsg(logFile, "%,*T\n", displayDepth, w);
  flushOut();
}

static retCode showConstant(ioPo out, methodPo mtd, integer conIx) {
  return outMsg(out, " %,*T", displayDepth, nthArg(mtd->pool, conIx));
}

static retCode showFrame(ioPo out, stackPo stk, methodPo mtd, integer conIx) {
  termPo frameLit = nthArg(mtd->pool, conIx);
  int32 stackDp = 0;
  if (isString(frameLit)) {
    integer sigLen;
    const char *sig = strVal(frameLit, &sigLen);
    tryRet(typeSigArity(sig, sigLen, &stackDp));
  } else if (isInteger(frameLit))
    stackDp = (int32) integerVal(frameLit);
  if (stk != Null) {
    assert(stackDp == stackDepth(stk, mtd, stk->sp, stk->fp));
    return outMsg(out, " %d %,*T", stackDp, displayDepth, frameLit);
  } else
    return outMsg(out, " %,*T", displayDepth, frameLit);
}

static retCode showSig(ioPo out, stackPo stk, methodPo mtd, integer conIx) {
  termPo frameLit = nthArg(mtd->pool, conIx);
  if (isString(frameLit)) {
    integer sigLen;
    const char *sig = strVal(frameLit, &sigLen);

    return outMsg(out, " %,*Y", sigLen, sig);
  } else
    return outMsg(out, " %,*T", displayDepth, frameLit);
}

// Figuring out if we should stop is surprisingly complicated
static logical shouldWeStop(processPo p, termPo arg) {
  if (focus == NULL || focus == p) {
    stackPo stk = p->stk;
    framePo frame = currFrame(stk);

    if (debugDebugging) {
      outMsg(logFile, "debug: waterMark=0x%x, sp=0x%x, fp=0x%x, traceCount=%d, tracing=%s, ins: ", p->waterMark,
             p->stk->sp, frame, p->traceCount, (p->tracing ? "yes" : "no"));
      disass(logFile, stk, frameMtd(frame), frame->pc);
      outMsg(logFile, "\n%_");
    }

    switch (frame->pc->op) {
      case Abort:
        return True;
      case Ret: {
        switch (p->waitFor) {
          case stepOut:
            if ((p->waterMark == frame && p->traceCount == 0)) {
              p->waterMark = Null;
              return True;
            } else
              return False;
          case stepInto:
          case stepOver:
            if (p->traceCount > 0)
              p->traceCount--;
            return (logical) (p->traceCount == 0);
          default:
            return False;
        }
      }
      case Entry: {
        if (p->waterMark == Null && breakPointSet(frameLbl(frame))) {
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
              return (logical) (p->traceCount == 0);
            default:
              return False;
          }
      }

      default: {
        switch (p->waitFor) {
          case stepInto:
            if (p->traceCount > 0)
              p->traceCount--;
            return (logical) (p->traceCount == 0);
          case stepOver:
//            return (logical) (p->traceCount == 0);
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

static DebugWaitFor cmder(debugOptPo opts, processPo p, methodPo mtd) {
  static strBufferPo cmdBuffer = Null;

  if (cmdBuffer == Null)
    cmdBuffer = newStringBuffer();

  if (interactive) {
    outMsg(debugOutChnnl, "\n[%d]>%s %_", processNo(p), (insDebugging ? "i" : lineDebugging ? "$" : ""));
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
            return opts->opts[ix].cmd(&cmdLine[nxt], p, opts->opts[ix].cl);
        }
        outMsg(debugOutChnnl, "invalid debugger command: %s\n", cmdLine);
      }
      default:
        for (int ix = 0; ix < opts->count; ix++)
          outMsg(debugOutChnnl, "%s\n", opts->opts[ix].usage);
        flushIo(debugOutChnnl);
        return moreDebug;
    }
  }
  return moreDebug;
}

static DebugWaitFor dbgSingle(char *line, processPo p, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->waterMark = Null;
  p->tracing = (logical) (p->traceCount == 0);
  return stepInto;
}

static DebugWaitFor dbgOver(char *line, processPo p, void *cl) {
  p->traceCount = cmdCount(line, 0);
  stackPo stk = p->stk;
  framePo frame = currFrame(stk);

  switch (frame->pc->op) {
    case Ret: {
      p->waterMark = previousFrame(stk, frame);
      break;
    }

    default:
      p->waterMark = frame;
      break;
  }
  p->tracing = False;

  resetDeflt("N");
  return stepOver;
}

static DebugWaitFor dbgQuit(char *line, processPo p, void *cl) {
  return quitDbg;
}

static DebugWaitFor dbgTrace(char *line, processPo p, void *cl) {
  p->tracing = True;
  p->traceCount = cmdCount(line, 0);

  resetDeflt("n");
  if (p->traceCount != 0)
    return stepInto;
  else
    return nextBreak;
}

static DebugWaitFor dbgCont(char *line, processPo p, void *cl) {
  p->tracing = False;

  resetDeflt("n");
  return nextBreak;
}

static DebugWaitFor dbgUntilRet(char *line, processPo p, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->tracing = False;
  resetDeflt("n");
  stackPo stk = p->stk;
  framePo frame = currFrame(stk);

  switch (frame->pc->op) {
    case Ret: {
      p->waterMark = previousFrame(stk, frame);
      break;
    }

    default:
      p->waterMark = frame;
      break;
  }
  return stepOut;
}

static DebugWaitFor dbgSetDepth(char *line, processPo p, void *cl) {
  integer depth = cmdCount(line, -1);
  if (depth >= 0)
    displayDepth = cmdCount(line, 0);

  outMsg(debugOutChnnl, "display depth %ld\n%_", displayDepth);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowRegisters(char *line, processPo p, void *cl) {
  showRegisters(p, p->heap);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowCall(char *line, processPo p, void *cl) {
  stackPo stk = p->stk;
  showStackCall(debugOutChnnl, displayDepth, stk->fp, stk, 0, showLocalVars);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowArg(char *line, processPo p, void *cl) {
  integer argNo = cmdCount(line, 0);
  stackPo stk = p->stk;
  framePo fp = stk->fp;
  methodPo mtd = frameMtd(fp);

  if (argNo >= 0 && argNo < argCount(mtd))
    showArg(debugOutChnnl, stk, argNo);
  else
    outMsg(debugOutChnnl, "invalid argument: %d", argNo);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowLocal(char *line, processPo p, void *cl) {
  integer lclNo = cmdCount(line, 0);
  stackPo stk = p->stk;
  framePo fp = stk->fp;
  methodPo mtd = frameMtd(fp);

  if (lclNo == 0)
    showAllLocals(debugOutChnnl, stk, fp);
  else if (lclNo > 0 && lclNo <= lclCount(mtd))
    showLcl(debugOutChnnl, stk, cmdCount(line, 0));
  else
    outMsg(debugOutChnnl, "invalid local number: %d", lclNo);

  resetDeflt("n");
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

static DebugWaitFor dbgShowStack(char *line, processPo p, void *cl) {
  stackPo stk = p->stk;
  framePo fp = currFrame(stk);
  ptrPo limit = stackLcl(fp, lclCount(frameMtd(fp)));
  ptrPo sp = stk->sp;

  if (line[0] == '\n') {

    for (integer vx = 0; sp < limit; vx++, sp++) {
      outMsg(debugOutChnnl, "SP[%d]=%,*T\n", vx, displayDepth, *sp);
    }
  } else {
    integer count = cmdCount(line, 1);
    limit -= count;

    for (integer vx = 0; sp < limit; vx++, sp++) {
      outMsg(debugOutChnnl, "SP[%d]=%,*T\n", vx, displayDepth, *sp);
    }
  }

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgStackTrace(char *line, processPo p, void *cl) {
  if (line[0] == 'L') {
    integer count = cmdCount(line+1, MAX_INT);

    stackTrace(p, debugOutChnnl, p->stk, displayDepth, showLocalVars, count);
  }else {
    integer count = cmdCount(line, MAX_INT);

    stackTrace(p, debugOutChnnl, p->stk, displayDepth, showArguments, count);
  }

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowCode(char *line, processPo p, void *cl) {
  stackPo stk = p->stk;
  framePo f = currFrame(stk);
  methodPo mtd = frameMtd(f);
  insPo pc = f->pc;
  integer remaining = codeSize(mtd) - (pc - entryPoint(mtd));

  integer count = cmdCount(line, remaining);
  insPo last = entryPoint(mtd) + codeSize(mtd);

  for (integer ix = 0; ix < count && pc < last; ix++) {
    pc = disass(debugOutChnnl, Null, mtd, pc);
    outStr(debugOutChnnl, "\n");
  }

  flushIo(debugOutChnnl);
  resetDeflt("n");

  return moreDebug;
}

void showMethodCode(ioPo out, char *msg, char *name, methodPo mtd) {
  insPo pc = entryPoint(mtd);
  insPo last = entryPoint(mtd) + codeSize(mtd);

  outMsg(out, msg, name);

  outMsg(out, "%d locals\n", lclCount(mtd));

  while (pc < last) {
    pc = disass(out, NULL, mtd, pc);
    outMsg(out, "\n");
  }
  flushOut();
}

static DebugWaitFor dbgDebug(char *line, processPo p, void *cl) {
  debugDebugging = !debugDebugging;

  logMsg(Stderr(), "debug debugging %s\n", (debugDebugging ? "enabled" : "disabled"));
  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgInsDebug(char *line, processPo p, void *cl) {
  lineDebugging = False;
  insDebugging = True;
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgSymbolDebug(char *line, processPo p, void *cl) {
  lineDebugging = True;
  insDebugging = False;
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgVerifyProcess(char *line, processPo p, void *cl) {
  resetDeflt("n");
  verifyProc(p, processHeap(p));
  return moreDebug;
}

static DebugWaitFor dbgAddBreakPoint(char *line, processPo p, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok) {
    integer count = addBreakPoints(&bp);
    outMsg(debugOutChnnl, "%d break points set\n%_", count);
  } else
    outMsg(debugOutChnnl, "could not set spy points on %s\n%_", line);
  return moreDebug;
}

DebugWaitFor dbgClearBreakPoint(char *line, processPo p, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok) {
    integer count = clearBreakPoints(&bp);
    outMsg(debugOutChnnl, "%d break points cleared\n%_", count);
  } else
    outMsg(debugOutChnnl, "could not clear spy points on %s\n%_", line);
  return moreDebug;
}

DebugWaitFor dbgShowBreakPoints(char *line, processPo p, void *cl) {
  retCode ret = showAllBreakPoints(debugOutChnnl);

  if (ret != Ok)
    outMsg(debugOutChnnl, "Could not show break points\n%_");
  return moreDebug;
}

static DebugWaitFor dbgDropFrame(char *line, processPo p, void *cl) {
  integer count = cmdCount(line, 0);
  stackPo stk = p->stk;

  integer frameNo = 0;
  framePo frame = stk->fp;

  while (frameNo < count && validFP(stk, frame)) {
    frame = dropFrame(stk);
    frameNo++;
  }

  if (frame != Null)
    frame->pc = entryPoint(frameMtd(frame));

  outMsg(debugOutChnnl, "Dropped %d frames\n%_", frameNo);
  showStackCall(debugOutChnnl, displayDepth, stk->fp, stk, 0, showPrognames);

  resetDeflt("n");
  return moreDebug;
}

static logical shouldWeStopIns(processPo p) {
  stackPo stk = p->stk;
  if (focus == NULL || focus == p) {
    framePo f = currFrame(stk);
#ifdef TRACE_DBG
    if (debugDebugging) {
      outMsg(logFile, "debug: waterMark=0x%x, fp=0x%x, traceCount=%d, tracing=%s, displayDepth=%d, ins: ",
             p->waterMark,
             p->stk->fp, p->traceCount, (p->tracing ? "yes" : "no"), displayDepth);
      disass(logFile, stk, frameMtd(f), f->pc);
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

    switch (f->pc->op) {
      case Ret: {
        switch (p->waitFor) {
          case stepOut:
            if ((p->waterMark == f && p->traceCount == 0)) {
              p->waterMark = Null;
              return True;
            } else
              return False;
          default:
            return False;
        }
      }
      case Entry: {
        if (p->waterMark == Null && breakPointSet(mtdLabel(frameMtd(p->stk->fp)))) {
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
      {.c = 'a', .cmd=dbgShowArg, .usage="a show argument variable"},
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
      {.c = 'v', .cmd=dbgVerifyProcess, .usage="v verify process"},
      {.c = '&', .cmd=dbgDebug, .usage="& flip debug debugging"}},
    .count = 22,
    .deflt = Null
  };

  logical stopping = shouldWeStopIns(p);
  if (p->tracing || stopping) {
    stackPo stk = p->stk;
    outMsg(debugOutChnnl, "[(%d)%ld]: ", stackNo(stk), pcCount);

    disass(debugOutChnnl, stk, frameMtd(stk->fp), stk->fp->pc);

    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0)
          p->waitFor = cmder(&opts, p, frameMtd(stk->fp));
        else {
          outStr(debugOutChnnl, "\n");
        }

        flushIo(debugOutChnnl);
        stackSanityCheck(p->stk);

        switch (p->waitFor) {
          case moreDebug:
            continue;
          case stepInto:
          case stepOver:
          case stepOut:
          case nextBreak:
          case never:
            return p->waitFor;
          case quitDbg:
            exit(0);
        }
      }
    } else {
      outStr(debugOutChnnl, "\n");
      flushIo(debugOutChnnl);
    }
  }
  return p->waitFor;
}

retCode showLoc(ioPo f, void *data, long depth, long precision, logical alt) {
  termPo ln = (termPo) data;

  if (ln != Null) {

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
  } else
    return outStr(f, "?unknown loc?");
}

static retCode shArgs(ioPo out, integer depth, ptrPo sp, integer arity) {
  char *sep = "";
  tryRet(outStr(out, "("));
  for (integer ix = 0; ix < arity; ix++) {
    tryRet(outMsg(out, "%s%#,*T", sep, depth, sp[ix]));
    sep = ", ";
  }
  return outMsg(out, ")");
}

static void showCall(ioPo out, stackPo stk, termPo pr) {
  framePo f = currFrame(stk);
  methodPo mtd = frameMtd(f);
  termPo loc = findPcLocation(mtd, codeOffset(mtd, f->pc));

  if (isALabel(pr)) {
    methodPo callee = labelCode(C_LBL(pr));

    if (showColors)
      outMsg(out, GREEN_ESC_ON"call:"GREEN_ESC_OFF" %#L %#.16T", loc, callee);
    else
      outMsg(out, "call: %#L %#.16T", loc, callee);

    shArgs(out, displayDepth, stk->fp->args, codeArity(callee));
  } else
    outMsg(out, "invalid use of showCall");
}

void showEntry(ioPo out, stackPo stk, termPo _call) {
  framePo f = currFrame(stk);
  methodPo mtd = frameMtd(f);
  termPo loc = findPcLocation(mtd, codeOffset(mtd, f->pc));

  if (showColors)
    outMsg(out, GREEN_ESC_ON"entry:"GREEN_ESC_OFF" %#L %#.16T", loc, mtd);
  else
    outMsg(out, "entry: %#L %#.16T", loc, mtd);

  shArgs(out, displayDepth, stk->fp->args, codeArity(mtd));
}

void showRet(ioPo out, stackPo stk, termPo val) {
  framePo f = currFrame(stk);

  if (showColors)
    outMsg(out, RED_ESC_ON"return:"RED_ESC_OFF" %T->%#,*T", frameMtd(f), displayDepth, val);
  else
    outMsg(out, "return: %T->%#,*T", frameMtd(f), displayDepth, val);
}

static void showAbort(ioPo out, stackPo stk, termPo reason) {
  framePo f = currFrame(stk);
  methodPo mtd = frameMtd(f);
  termPo loc = findPcLocation(mtd, codeOffset(mtd, f->pc));

  if (showColors)
    outMsg(out, RED_ESC_ON"abort:"RED_ESC_OFF" %L %T->%#,*T", loc, frameMtd(f), displayDepth, reason);
  else
    outMsg(out, "abort: %L %T->%#,*T", loc, frameMtd(f), displayDepth, reason);
}

void showAssign(ioPo out, stackPo stk, termPo vl) {
  framePo f = currFrame(stk);
  methodPo mtd = frameMtd(f);
  termPo loc = findPcLocation(mtd, codeOffset(mtd, f->pc));
  termPo val = peekStack(stk, 1);
  termPo cell = topStack(stk);

  if (showColors)
    outMsg(out, RED_ESC_ON"assign:"RED_ESC_OFF" %L %T->%#,*T", loc, cell, displayDepth, val);
  else
    outMsg(out, "assign: %L %T->%#,*T", loc, cell, displayDepth, val);
}

void showSuspend(ioPo out, stackPo stk, termPo cont) {
  framePo f = currFrame(stk);
  methodPo mtd = frameMtd(f);
  termPo loc = findPcLocation(mtd, codeOffset(mtd, f->pc));

  if (showColors)
    outMsg(out, CYAN_ESC_ON"suspend:"CYAN_ESC_OFF "%L %#,*T", loc, displayDepth, cont);
  else
    outMsg(out, "suspend:", "%L %#,*T", loc, displayDepth, cont);
}

void showResume(ioPo out, stackPo stk, termPo cont) {
  framePo f = currFrame(stk);
  methodPo mtd = frameMtd(f);
  termPo loc = findPcLocation(mtd, codeOffset(mtd, f->pc));

  if (showColors)
    outMsg(out, CYAN_ESC_ON"resume:"CYAN_ESC_OFF "%L %#,*T", loc, displayDepth, cont);
  else
    outMsg(out, "resume:", "%L %#,*T", loc, displayDepth, cont);
}

void showRetire(ioPo out, stackPo stk, termPo cont) {
  framePo f = currFrame(stk);
  methodPo mtd = frameMtd(f);
  termPo loc = findPcLocation(mtd, codeOffset(mtd, f->pc));

  if (showColors)
    outMsg(out, CYAN_ESC_ON"retire:"CYAN_ESC_OFF "%l %#,*T", loc, displayDepth, cont);
  else
    outMsg(out, "retire:", "%L %#,*T", loc, displayDepth, cont);
}

typedef void (*showCmd)(ioPo out, stackPo stk, termPo trm);

static DebugWaitFor lnDebug(processPo p, termPo arg, showCmd show);

DebugWaitFor enterDebug(processPo p) {
  stackPo stk = p->stk;
  framePo f = currFrame(stk);
  insPo pc = f->pc;

  lineCount++;
  switch (pc->op) {
    case Abort:
      return lnDebug(p, peekStack(stk, 1), showAbort);
    case Call:
    case TCall:
      return lnDebug(p, getMtdLit(frameMtd(f), pc->fst), showCall);
    case Entry:
      return lnDebug(p, Null, showEntry);
    case Ret:
      return lnDebug(p, topStack(stk), showRet);
    case Assign:
      return lnDebug(p, Null, showAssign);
    case Suspend:
      return lnDebug(p, topStack(stk), showSuspend);
    case Resume:
      return lnDebug(p, topStack(stk), showResume);
    case Retire:
      return lnDebug(p, topStack(stk), showRetire);
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
    {.c = 'a', .cmd=dbgShowArg, .usage="a show argument variable"},
    {.c = 'l', .cmd=dbgShowLocal, .usage="l show local variable"},
    {.c = 'i', .cmd=dbgShowCode, .usage="i show instructions"},
    {.c = 'd', .cmd=dbgSetDepth, .usage="d <dpth> set display depth"},
    {.c = '+', .cmd=dbgAddBreakPoint, .usage="+ add break point"},
    {.c = '-', .cmd=dbgClearBreakPoint, .usage="- clear break point"},
    {.c = 'B', .cmd=dbgShowBreakPoints, .usage="show all break points"},
    {.c = 'y', .cmd=dbgInsDebug, .usage="y turn on instruction mode"},
    {.c = '&', .cmd=dbgDebug, .usage="& flip debug debugging"}},
    .count = 21,
    .deflt = Null
  };

  stackPo stk = p->stk;
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
          p->waitFor = cmder(&opts, p, frameMtd(currFrame(stk)));
        } else {
          outStr(debugOutChnnl, "\n");
          flushIo(debugOutChnnl);
        }

        switch (p->waitFor) {
          case moreDebug:
            continue;
          case stepInto:
          case stepOver:
          case stepOut:
          case nextBreak:
          case never:
            return p->waitFor;
          case quitDbg:
            exit(0);
        }
      }
    } else {
      outStr(debugOutChnnl, "\n");
      flushIo(debugOutChnnl);
    }
  }
  return p->waitFor;
}

void stackSummary(ioPo out, stackPo stk) {
  integer freeCount = stk->sp - ((ptrPo) (stk->fp + 1));
  integer used = stk->sze - freeCount;
  outMsg(out, "used: %l, free:%5.2g%%", used, ((double) freeCount) / (double) stk->sze);
}

retCode showArg(ioPo out, stackPo stk, integer arg) {
  if (stk != Null) {
    return outMsg(out, " A[%d] = %,*T", arg, displayDepth, *stackArg(stk->fp, arg));
  } else
    return outMsg(out, " A[%d]", arg);
}

void showAllLocals(ioPo out, stackPo stk, framePo fp) {
  methodPo mtd = frameMtd(fp);
  for (integer vx = 1; vx <= lclCount(mtd); vx++) {
    char vName[MAX_SYMB_LEN];
    if (localVName(mtd, fp->pc, vx, vName, NumberOf(vName)) == Ok) {
      ptrPo var = stackLcl(fp, vx);
      if (*var != Null && *var != voidEnum)
        outMsg(out, "  %s(%d) = %#,*T\n", vName, vx, displayDepth, *var);
      else
        outMsg(out, "  %s(%d) (unset)\n", vName, vx);
    } else {
      ptrPo var = stackLcl(fp, vx);
      if (*var != Null)
        outMsg(out, "  L[%d] = %#,*T\n", vx, displayDepth, *var);
    }
  }
}

retCode showLcl(ioPo out, stackPo stk, integer vr) {
  if (stk != Null)
    return outMsg(out, " l[%d] = %,*T", vr, displayDepth, *stackLcl(stk->fp, vr));
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

void showTos(ioPo out, stackPo stk, integer offset) {
  if (stk != Null) {
    if (offset == 0)
      outMsg(out, " tos = %#,*T", displayDepth, peekStack(stk, 0));
    else
      outMsg(out, " tos[%d] = %#,*T", offset, displayDepth, peekStack(stk, offset));
  } else
    outMsg(out, " tos");
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

static void showPcTgt(ioPo out, int32 pc, int32 offset) {
  if (pc >= 0)
    outMsg(out, " ↓[%d]", pc + offset);
  else
    outMsg(out, " ↓%d", offset);
}

static void showBlkLvl(ioPo out, int32 pc, int32 offset) {
  if (pc >= 0)
    outMsg(out, " ↑[%d]", pc + 1 + offset);
  else
    outMsg(out, " ↑%d", offset);
}

static void showEscCall(ioPo out, int32 escNo) {
  escapePo esc = getEscape(escNo);
  outMsg(out, " %s/%d", esc->name, esc->arity);
}

insPo disass(ioPo out, stackPo stk, methodPo mtd, insPo pc) {
  int32 offset = -1;
  if (mtd != Null) {
    offset = codeOffset(mtd, pc);

    normalPo lits = codeLits(mtd);
    if (lits != Null)
      outMsg(out, "%,*T [%d] ", displayDepth, nthArg(lits, 0), offset);
    else
      outMsg(out, "\?\? [%d] ", offset);
  } else {
    outMsg(out, "\?\?\? [%lx] ", pc);
  }

  switch (pc->op) {
#undef instruction

#define show_nOp(Tgt)
#define show_tOs(Tgt) showTos(out,stk,delta++)
#define show_art(Tgt) outMsg(out," /%d",(Tgt))
#define show_i32(Tgt) outMsg(out," #%d",(Tgt))
#define show_arg(Tgt) showArg(out,stk,(Tgt))
#define show_lcl(Tgt) showLcl(out,stk,(Tgt))
#define show_lcs(Tgt) outMsg(out," l[%d]",(Tgt))
#define show_bLk(Tgt) showPcTgt(out,offset,(Tgt)+1)
#define show_lVl(Tgt) showBlkLvl(out,offset,(Tgt))
#define show_sym(Tgt) showConstant(out,mtd,(Tgt))
#define show_Es(Tgt) showEscCall(out, (Tgt))
#define show_lit(Tgt) showConstant(out,mtd,(Tgt))
#define show_lNe(Tgt) showConstant(out,mtd,(Tgt))
#define show_glb(Tgt) showGlb(out, findGlobalVar((Tgt)))
#define show_tPe(Tgt) showSig(out,stk,mtd,(Tgt))

#define instruction(Op, A1, A2, Dl, _, Cmt)\
    case Op:{                               \
      outMsg(out," %s",#Op);                \
      integer delta=0;                      \
      show_##A1(pc->fst);                   \
      show_##A2(pc->alt);                   \
      return pc+1;                          \
    }

#include "instructions.h"

    default:
      return pc + 1;
  }
}

retCode dissassMtd(ioPo out, stackPo stk, methodPo mtd, integer precision, integer depth, logical alt, char *prefix) {
  insPo code = entryPoint(mtd);
  insPo limit = code + codeSize(mtd);
  for (insPo pc = code; pc < limit; pc++) {
    disass(out, stk, mtd, pc);
    outMsg(logFile, "\n");
  }
  return Ok;
}

void showRegisters(processPo p, heapPo h) {
  stackPo stk = p->stk;
  framePo fp = stk->fp;
  methodPo mtd = frameMtd(fp);
  ptrPo limit = stackLcl(fp, lclCount(mtd));
  ptrPo sp = stk->sp;

  for (integer vx = 0; sp < limit; vx++) {
    outMsg(debugOutChnnl, "SP[%d]=%,*T\n", vx, displayDepth, *sp++);
  }

  integer count = argCount(mtd);
  for (integer ix = 0; ix < count; ix++) {
    outMsg(debugOutChnnl, "A[%d]=%,*T\n", ix, displayDepth, *stackArg(fp, ix));
  }

  showAllLocals(debugOutChnnl, stk, fp);

#ifdef TRACE_DBG
  if (debugDebugging) {
    stackSummary(debugOutChnnl, stk);
    heapSummary(debugOutChnnl, h);
  }
#endif
  outMsg(debugOutChnnl, "\n%_");
}

static char *anonPrefix = "__";

retCode localVName(methodPo mtd, insPo pc, integer vNo, char *buffer, integer bufLen) {
//  normalPo locals = mtd->locals;
//  int64 numLocals = termArity(locals);
//  integer pcOffset = codeOffset(mtd, pc);
//
//  for (int32 ix = 0; ix < numLocals; ix++) {
//    normalPo vr = C_NORMAL(nthArg(locals, ix));
//    integer from = integerVal(nthArg(vr, 1));
//    integer to = integerVal(nthArg(vr, 2));
//
//    if (from <= pcOffset && to > pcOffset && integerVal(nthArg(vr, 3)) == vNo) {
//      copyChars2Buff(C_STR(nthArg(vr, 0)), buffer, bufLen);
//
//      if (uniIsLitPrefix(buffer, anonPrefix))
//        uniCpy(buffer, bufLen, "l");
//      return Ok;
//    }
//  }
  return Fail;
}

static retCode swapIndex(integer i, integer j, void *cl) {
  integer *indices = (integer *) cl;
  integer w = indices[i];
  indices[i] = indices[j];
  indices[j] = w;
  return Ok;
}

void dumpStats() {
  dumpOpCount(debugOutChnnl);
  dumpEscapes(debugOutChnnl);
  showMtdCounts(debugOutChnnl);
  dumpStackStats(debugOutChnnl);
  dumpGcStats(debugOutChnnl);
}
