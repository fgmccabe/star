// Incremental instruction debugger

#include "engineP.h"
#include <stdlib.h>
#include <globals.h>
#include "constants.h"
#include <sock.h>
#include <manifest.h>

#include "debugP.h"
#include "arith.h"
#include "editline.h"
#include "escapeP.h"

integer pcCount = 0;

static void showLine(ioPo out, stackPo stk, termPo lc, termPo ignore);
static void showEntry(ioPo out, stackPo stk, termPo lc, termPo call);
static void showAbort(ioPo out, stackPo stk, termPo lc, termPo reason);
static void showRet(ioPo out, stackPo stk, termPo lc, termPo val);
static void showXRet(ioPo out, stackPo stk, termPo lc, termPo val);
static void showAssign(ioPo out, stackPo stk, termPo lc, termPo vl);
static void showResume(ioPo out, stackPo stk, termPo lc, termPo cont);
static void showRegisters(processPo p, heapPo h);
static void showAllLocals(ioPo out, stackPo stk);
static void showTos(ioPo out, stackPo stk, integer offset);

static retCode showLcl(ioPo out, ptrPo args, int32 vr);
static retCode showArg(ioPo out, ptrPo args, integer arg);
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

ReturnStatus g__ins_debug(processPo P) {
  insDebugging = tracing = True;
  P->waitFor = stepInto;
  P->tracing = True;
  P->traceCount = 0;
  P->waterMark = P->stk->fp;

  pshVal(P, unitEnum);
  return Normal;
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

static retCode showConstant(ioPo out, methodPo mtd, int32 conIx) {
  return outMsg(out, " %,*T", displayDepth, getConstant(conIx));
}

static retCode showSig(ioPo out, stackPo stk, methodPo mtd, int32 conIx) {
  termPo frameLit = getConstant(conIx);
  if (isString(frameLit)) {
    integer sigLen;
    const char *sig = strVal(frameLit, &sigLen);

    return outMsg(out, " %,*Y", sigLen, sig);
  } else
    return outMsg(out, " %,*T", displayDepth, frameLit);
}

// Figuring out if we should stop is surprisingly complicated
static logical shouldWeStop(processPo p, OpCode op) {
  stackPo stk = p->stk;
  if (focus == NULL || focus == p) {
    framePo f = currFrame(stk);
#ifdef TRACE_DBG
    if (debugDebugging) {
      outMsg(logFile, "debug: waterMark=0x%x, fp=0x%x, traceCount=%d, tracing=%s, displayDepth=%d, ins: ",
             p->waterMark, f, p->traceCount, (p->tracing ? "yes" : "no"), displayDepth);
      disass(logFile, stk, stk->prog, stk->pc);
      outMsg(logFile, "\n%_");
    }
#endif
    switch (p->waitFor) {
      case stepInto:
        if (p->traceCount > 0)
          p->traceCount--;
        return (logical) (p->traceCount == 0);

      case stepOver:
      case stepOut: {
        if (p->waterMark == f) {
          if (p->traceCount > 0)
            p->traceCount--;
          if (p->traceCount == 0) {
            p->waterMark = Null;
            return True;
          } else
            return False;
        } else
          return False;
      }
      case nextBreak: {
        if (p->waterMark == Null && op == Entry && breakPointSet(mtdLabel(stk->prog))) {
          p->waitFor = stepInto;
          p->tracing = True;
          return True;
        }
        return False;
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

static DebugWaitFor cmder(debugOptPo opts, processPo p, termPo lc) {
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
            return opts->opts[ix].cmd(&cmdLine[nxt], p, lc, opts->opts[ix].cl);
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

static DebugWaitFor dbgSingle(char *line, processPo p, termPo lc, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->waterMark = Null;
  p->tracing = (logical) (p->traceCount == 0);
  return stepInto;
}

static DebugWaitFor dbgOver(char *line, processPo p, termPo lc, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->waterMark = p->stk->fp;
  p->tracing = False;

  resetDeflt("N");
  return stepOver;
}

static DebugWaitFor dbgUntilRet(char *line, processPo p, termPo lc, void *cl) {
  p->traceCount = cmdCount(line, 0);
  p->tracing = False;
  resetDeflt("n");
  p->waterMark = previousFrame(p->stk, p->stk->fp);

  return stepOut;
}

static DebugWaitFor dbgQuit(char *line, processPo p, termPo lc, void *cl) {
  return quitDbg;
}

static DebugWaitFor dbgTrace(char *line, processPo p, termPo lc, void *cl) {
  p->tracing = True;
  p->traceCount = cmdCount(line, 0);

  resetDeflt("n");
  if (p->traceCount != 0)
    return stepInto;
  else
    return nextBreak;
}

static DebugWaitFor dbgCont(char *line, processPo p, termPo lc, void *cl) {
  p->tracing = False;

  resetDeflt("n");
  return nextBreak;
}

static DebugWaitFor dbgSetDepth(char *line, processPo p, termPo lc, void *cl) {
  integer depth = cmdCount(line, -1);
  if (depth >= 0)
    displayDepth = cmdCount(line, 0);

  outMsg(debugOutChnnl, "display depth %ld\n%_", displayDepth);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowRegisters(char *line, processPo p, termPo lc, void *cl) {
  showRegisters(p, p->heap);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowCall(char *line, processPo p, termPo lc, void *cl) {
  stackPo stk = p->stk;
  showStackCall(debugOutChnnl, displayDepth, stk->args, 0, showLocalVars, stk->prog, stk->pc);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowArg(char *line, processPo p, termPo lc, void *cl) {
  integer argNo = cmdCount(line, 0);
  stackPo stk = p->stk;
  methodPo mtd = stk->prog;

  if (argNo >= 0 && argNo < argCount(mtd))
    showArg(debugOutChnnl, stk->args, argNo);
  else
    outMsg(debugOutChnnl, "invalid argument: %d", argNo);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowLocal(char *line, processPo p, termPo lc, void *cl) {
  int32 lclNo = (int32) cmdCount(line, 0);
  stackPo stk = p->stk;
  methodPo mtd = stk->prog;

  if (lclNo == 0)
    showAllLocals(debugOutChnnl, stk);
  else if (lclNo > 0 && lclNo <= lclCount(mtd))
    showLcl(debugOutChnnl, stk->args, lclNo);
  else
    outMsg(debugOutChnnl, "invalid local number: %d", lclNo);

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowGlobal(char *line, processPo p, termPo lc, void *cl) {
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

static DebugWaitFor dbgShowStack(char *line, processPo p, termPo lc, void *cl) {
  stackPo stk = p->stk;
  ptrPo limit = stackLcl(stk->args, lclCount(stk->prog));
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

static DebugWaitFor dbgStackTrace(char *line, processPo p, termPo lc, void *cl) {
  if (line[0] == 'L') {
    integer count = cmdCount(line + 1, MAX_INT);

    stackTrace(p, debugOutChnnl, p->stk, displayDepth, showLocalVars, count);
  } else {
    integer count = cmdCount(line, MAX_INT);

    stackTrace(p, debugOutChnnl, p->stk, displayDepth, showArguments, count);
  }

  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgShowCode(char *line, processPo p, termPo lc, void *cl) {
  stackPo stk = p->stk;
  methodPo mtd = stk->prog;
  insPo pc = stk->pc;
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

void showMethodCode(ioPo out, char *msg, methodPo mtd) {
  insPo pc = entryPoint(mtd);
  insPo last = entryPoint(mtd) + codeSize(mtd);

  outMsg(out, msg, mtdLabel(mtd));

  outMsg(out, "%d locals\n", lclCount(mtd));

  while (pc < last) {
    pc = disass(out, NULL, mtd, pc);
    outMsg(out, "\n");
  }
  flushOut();
}

static DebugWaitFor dbgDebug(char *line, processPo p, termPo lc, void *cl) {
  debugDebugging = !debugDebugging;

  logMsg(Stderr(), "debug debugging %s\n", (debugDebugging ? "enabled" : "disabled"));
  resetDeflt("n");
  return moreDebug;
}

static DebugWaitFor dbgInsDebug(char *line, processPo p, termPo lc, void *cl) {
  lineDebugging = False;
  insDebugging = True;
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgSymbolDebug(char *line, processPo p, termPo lc, void *cl) {
  lineDebugging = True;
  insDebugging = False;
  resetDeflt("n");
  return stepInto;
}

static DebugWaitFor dbgVerifyProcess(char *line, processPo p, termPo lc, void *cl) {
  resetDeflt("n");
  verifyProc(p, processHeap(p));
  return moreDebug;
}

static DebugWaitFor dbgAddBreakPoint(char *line, processPo p, termPo lc, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok) {
    integer count = addBreakPoints(&bp);
    outMsg(debugOutChnnl, "%d break points set\n%_", count);
  } else
    outMsg(debugOutChnnl, "could not set spy points on %s\n%_", line);
  return moreDebug;
}

DebugWaitFor dbgClearBreakPoint(char *line, processPo p, termPo lc, void *cl) {
  BreakPoint bp;
  retCode ret = parseBreakPoint(line, uniStrLen(line), &bp);
  if (ret == Ok) {
    integer count = clearBreakPoints(&bp);
    outMsg(debugOutChnnl, "%d break points cleared\n%_", count);
  } else
    outMsg(debugOutChnnl, "could not clear spy points on %s\n%_", line);
  return moreDebug;
}

DebugWaitFor dbgShowBreakPoints(char *line, processPo p, termPo lc, void *cl) {
  retCode ret = showAllBreakPoints(debugOutChnnl);

  if (ret != Ok)
    outMsg(debugOutChnnl, "Could not show break points\n%_");
  return moreDebug;
}

static DebugWaitFor dbgDropFrame(char *line, processPo p, termPo lc, void *cl) {
  integer count = cmdCount(line, 0);
  stackPo stk = p->stk;

  integer frameNo = 0;
  framePo frame = stk->fp;

  while (frameNo < count && validFP(stk, frame)) {
    frame = dropFrame(stk);
    frameNo++;
  }

  stk->pc = entryPoint(stk->prog);

  outMsg(debugOutChnnl, "Dropped %d frames\n%_", frameNo);
  showStackCall(debugOutChnnl, displayDepth, stk->args, 0, showPrognames, stk->prog, stk->pc);

  resetDeflt("n");
  return moreDebug;
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

  logical stopping = shouldWeStop(p, p->stk->pc->op);
  if (p->tracing || stopping) {
    stackPo stk = p->stk;
    outMsg(debugOutChnnl, "[(%d)%ld]: ", stackNo(stk), pcCount);

    disass(debugOutChnnl, stk, stk->prog, stk->pc);

    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0)
          p->waitFor = cmder(&opts, p, voidEnum);
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

static void showCall(ioPo out, stackPo stk, termPo lc, termPo pr) {
  if (isALabel(pr)) {
    labelPo callee = C_LBL(pr);

    if (showColors)
      outMsg(out, GREEN_ESC_ON"call:"GREEN_ESC_OFF" %#L %#.16T", lc, callee);
    else
      outMsg(out, "call: %#L %#.16A", lc, callee);

    shArgs(out, displayDepth, stk->sp, lblArity(callee));
  } else
    outMsg(out, "invalid use of showCall");
}

static void showOCall(ioPo out, stackPo stk, termPo lc, termPo closure) {
  if (showColors)
    outMsg(out, GREEN_ESC_ON"call:"GREEN_ESC_OFF" %#L %T", lc, closure);
  else
    outMsg(out, "call: %#L %T", lc, closure);

  if (isClosure(closure)) {
    labelPo pr = closureLabel(C_CLOSURE(closure));
    shArgs(out, displayDepth, stk->sp, lblArity(pr));
  } else {
    outMsg(out, "invalid closure label");
  }
}

static void showTCall(ioPo out, stackPo stk, termPo lc, termPo pr) {
  if (isALabel(pr)) {
    labelPo callee = C_LBL(pr);

    if (showColors)
      outMsg(out, GREEN_ESC_ON"tcall:"GREEN_ESC_OFF" %#L %#.16A", lc, callee);
    else
      outMsg(out, "tcall: %#L %#.16A", lc, callee);

    shArgs(out, displayDepth, stk->sp, lblArity(callee));
  } else
    outMsg(out, "invalid use of showTCall");
}

static void showLine(ioPo out, stackPo stk, termPo lc, termPo ignore) {
  if (showColors)
    outMsg(out, GREEN_ESC_ON"line:"GREEN_ESC_OFF" %#L", lc);
  else
    outMsg(out, "line: %#L", lc);
}

void showEntry(ioPo out, stackPo stk, termPo lc, termPo lbl) {
  if (showColors)
    outMsg(out, GREEN_ESC_ON"entry:"GREEN_ESC_OFF" %#L %#.16A", lc, lbl);
  else
    outMsg(out, "entry: %#L %#.16A", lc, lbl);

  shArgs(out, displayDepth, stk->args, lblArity(C_LBL(lbl)));
}

void showRet(ioPo out, stackPo stk, termPo lc, termPo val) {
  if (showColors)
    outMsg(out, RED_ESC_ON"return:"RED_ESC_OFF" %#L %T->%#,*T", lc, stk->prog, displayDepth, val);
  else
    outMsg(out, "return: %#L %T->%#,*T", lc, stk->prog, displayDepth, val);
}

void showXRet(ioPo out, stackPo stk, termPo lc, termPo val) {
  if (showColors)
    outMsg(out, RED_ESC_ON"throw:"RED_ESC_OFF" %#L %T->%#,*T", lc, stk->prog, displayDepth, val);
  else
    outMsg(out, "throw: %#L %T->%#,*T", lc, stk->prog, displayDepth, val);
}

static void showAbort(ioPo out, stackPo stk, termPo lc, termPo reason) {
  methodPo mtd = stk->prog;

  if (showColors)
    outMsg(out, RED_ESC_ON"abort:"RED_ESC_OFF" %#L %T->%#,*T", lc, mtd, displayDepth, reason);
  else
    outMsg(out, "abort: %L %T->%#,*T", lc, mtd, displayDepth, reason);
}

void showAssign(ioPo out, stackPo stk, termPo lc, termPo vl) {
  termPo val = peekStack(stk, 1);
  termPo cell = topStack(stk);

  if (showColors)
    outMsg(out, RED_ESC_ON"assign:"RED_ESC_OFF" %#L %T->%#,*T", lc, cell, displayDepth, val);
  else
    outMsg(out, "assign: %L %T->%#,*T", lc, cell, displayDepth, val);
}

void showFiber(ioPo out, stackPo stk, termPo lc, termPo cont) {
  if (showColors)
    outMsg(out, CYAN_ESC_ON"fiber:"CYAN_ESC_OFF "%L %#,*T", lc, displayDepth, cont);
  else
    outMsg(out, "fiber: %L %#,*T", lc, displayDepth, cont);
}

void showSuspend(ioPo out, stackPo stk, termPo lc, termPo cont) {
  if (showColors)
    outMsg(out, CYAN_ESC_ON"suspend:"CYAN_ESC_OFF "%L %#,*T", lc, displayDepth, cont);
  else
    outMsg(out, "suspend: %L %#,*T", lc, displayDepth, cont);
}

void showResume(ioPo out, stackPo stk, termPo lc, termPo cont) {
  if (showColors)
    outMsg(out, CYAN_ESC_ON"resume:"CYAN_ESC_OFF "%L %#,*T", lc, displayDepth, cont);
  else
    outMsg(out, "resume: %L %#,*T", lc, displayDepth, cont);
}

void showRetire(ioPo out, stackPo stk, termPo lc, termPo cont) {
  if (showColors)
    outMsg(out, CYAN_ESC_ON"retire:"CYAN_ESC_OFF "%L %#,*T", lc, displayDepth, cont);
  else
    outMsg(out, "retire: %L %#,*T", lc, displayDepth, cont);
}

typedef void (*showCmd)(ioPo out, stackPo stk, termPo lc, termPo trm);

static DebugWaitFor lnDebug(processPo p, OpCode op, termPo lc, termPo arg, showCmd show);

DebugWaitFor enterDebugger(processPo p, termPo lc) {
  stackPo stk = p->stk;
  insPo pc = stk->pc;

  switch (pc->op) {
    case Abort:
      return abortDebug(p, lc);
    case Call:
      return callDebug(p, Call, lc, getConstant(pc->fst));
    case XCall:
      return callDebug(p, XCall, lc, getConstant(pc->fst));
    case TCall:
      return tcallDebug(p, lc, getConstant(pc->fst));
    case OCall:
      return ocallDebug(p, OCall, lc, topStack(stk));
    case XOCall:
      return ocallDebug(p, XOCall, lc, topStack(stk));
    case TOCall:
      return tocallDebug(p, lc, topStack(stk));
    case Entry:
      return entryDebug(p, lc, mtdLabel(stk->prog));
    case Ret:
      return retDebug(p, lc, topStack(stk));
    case XRet:
      return xretDebug(p, lc, topStack(stk));
    case Assign:
      return assignDebug(p, lc);
    case Fiber:
      return fiberDebug(p, lc, topStack(stk));
    case Suspend:
      return suspendDebug(p, lc, topStack(stk));
    case Resume:
      return resumeDebug(p, lc, topStack(stk));
    case Retire:
      return retireDebug(p, lc, topStack(stk));
    default:
      return stepOver;
  }
}

DebugWaitFor lineDebug(processPo p, termPo lc) {
  return lnDebug(p, Line, lc, Null, showLine);
}

DebugWaitFor abortDebug(processPo p, termPo lc) {
  stackPo stk = p->stk;
  return lnDebug(p, Abort, lc, topStack(stk), showAbort);
}

DebugWaitFor callDebug(processPo p, OpCode op, termPo lc, termPo pr) {
  return lnDebug(p, op, lc, pr, showCall);
}

DebugWaitFor tcallDebug(processPo p, termPo lc, termPo pr) {
  return lnDebug(p, TCall, lc, pr, showTCall);
}

DebugWaitFor ocallDebug(processPo p, OpCode op, termPo lc, termPo pr) {
  return lnDebug(p, op, lc, pr, showOCall);
}

DebugWaitFor tocallDebug(processPo p, termPo lc, termPo pr) {
  return lnDebug(p, TOCall, lc, pr, showOCall);

}

DebugWaitFor entryDebug(processPo p, termPo lc, labelPo lbl) {
  return lnDebug(p, Entry, lc, (termPo)lbl, showEntry);
}

DebugWaitFor retDebug(processPo p, termPo lc, termPo vl) {
  return lnDebug(p, Ret, lc, vl, showRet);
}

DebugWaitFor xretDebug(processPo p, termPo lc, termPo vl) {
  return lnDebug(p, XRet, lc, vl, showXRet);
}

DebugWaitFor assignDebug(processPo p, termPo lc) {
  return lnDebug(p, Assign, lc, Null, showAssign);
}

DebugWaitFor fiberDebug(processPo p, termPo lc, termPo vl) {
  return lnDebug(p, Fiber, lc, vl, showFiber);
}

DebugWaitFor suspendDebug(processPo p, termPo lc, termPo vl) {
  return lnDebug(p, Suspend, lc, vl, showSuspend);
}

DebugWaitFor resumeDebug(processPo p, termPo lc, termPo vl) {
  return lnDebug(p, Assign, lc, vl, showResume);
}

DebugWaitFor retireDebug(processPo p, termPo lc, termPo vl) {
  return lnDebug(p, Retire, lc, vl, showRetire);
}

DebugWaitFor lnDebug(processPo p, OpCode op, termPo lc, termPo arg, showCmd show) {
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
  logical stopping = shouldWeStop(p, op);

#ifdef TRACE_DBG
  if (debugDebugging) {
    logMsg(logFile, "traceCount=%d, waterMark=%x, stopping=%s, tracing=%s", p->traceCount, p->waterMark,
           (stopping ? "yes" : "no"), (p->tracing ? "yes" : "no"));
  }
#endif
  if (p->tracing || stopping) {
    show(debugOutChnnl, stk, lc, arg);
    if (stopping) {
      while (interactive) {
        if (p->traceCount == 0) {
          p->waitFor = cmder(&opts, p, lc);
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

retCode showArg(ioPo out, ptrPo args, integer arg) {
  if (args != Null) {
    return outMsg(out, " A[%d] = %,*T", arg, displayDepth, *stackArg(args, arg));
  } else
    return outMsg(out, " A[%d]", arg);
}

void showAllLocals(ioPo out, stackPo stk) {
  methodPo mtd = stk->prog;
  for (int32 vx = 1; vx <= lclCount(mtd); vx++) {
    char vName[MAX_SYMB_LEN];
    if (localVName(mtd, stk->pc, vx, vName, NumberOf(vName)) == Ok) {
      ptrPo var = stackLcl(stk->args, vx);
      if (*var != Null && *var != voidEnum)
        outMsg(out, "  %s(%d) = %#,*T\n", vName, vx, displayDepth, *var);
      else
        outMsg(out, "  %s(%d) (unset)\n", vName, vx);
    } else {
      ptrPo var = stackLcl(stk->args, vx);
      if (*var != Null)
        outMsg(out, "  L[%d] = %#,*T\n", vx, displayDepth, *var);
    }
  }
}

retCode showLcl(ioPo out, ptrPo args, int32 vr) {
  if (args != Null)
    return outMsg(out, " l[%d] = %,*T", vr, displayDepth, *stackLcl(args, vr));
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
    labelPo lbl = mtdLabel(mtd);
    outMsg(out, "%,*T [%d] ", displayDepth, lbl, offset);
  } else {
    outMsg(out, "\?\?\? [%lx] ", pc);
  }

  ptrPo args = (stk != Null) ? stk->args : Null;

  switch (pc->op) {
#undef instruction

#define show_nOp(Tgt)
#define show_tOs(Tgt) showTos(out,stk,delta++)
#define show_art(Tgt) outMsg(out," /%d",(Tgt))
#define show_i32(Tgt) outMsg(out," #%d",(Tgt))
#define show_arg(Tgt) showArg(out,args,(Tgt))
#define show_lcl(Tgt) showLcl(out,args,(Tgt))
#define show_lcs(Tgt) outMsg(out," l[%d]",(Tgt))
#define show_bLk(Tgt) showPcTgt(out,offset,(Tgt)+1)
#define show_lVl(Tgt) showBlkLvl(out,offset,(Tgt))
#define show_sym(Tgt) showConstant(out,mtd,(Tgt))
#define show_Es(Tgt) showEscCall(out, (Tgt))
#define show_lit(Tgt) showConstant(out,mtd,(Tgt))
#define show_glb(Tgt) showGlb(out, findGlobalVar((Tgt)))
#define show_tPe(Tgt) showSig(out,stk,mtd,(Tgt))

#define instruction(Op, A1, A2, Dl, Cmt)\
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

void showRegisters(processPo p, heapPo h) {
  stackPo stk = p->stk;
  methodPo mtd = stk->prog;
  ptrPo limit = stackLcl(stk->args, lclCount(mtd));
  ptrPo sp = stk->sp;
  ptrPo args = stk->args;

  for (integer vx = 0; sp < limit; vx++) {
    outMsg(debugOutChnnl, "SP[%d]=%,*T\n", vx, displayDepth, *sp++);
  }

  integer count = argCount(mtd);
  for (integer ix = 0; ix < count; ix++) {
    outMsg(debugOutChnnl, "A[%d]=%,*T\n", ix, displayDepth, *stackArg(args, ix));
  }

  showAllLocals(debugOutChnnl, stk);

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
  dumpEscapes(debugOutChnnl);
  showMtdCounts(debugOutChnnl);
  dumpStackStats(debugOutChnnl);
  dumpGcStats(debugOutChnnl);
}
