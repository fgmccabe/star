// Incremental instruction debugger

#include "engineP.h"
#include "signature.h"
#include "libEscapes.h"
#include <stdlib.h>
#include <globals.h>

#include "debug.h"
#include "arith.h"
#include "term.h"

logical altDebug = False;
long maxDepth = MAX_INT;

typedef struct _break_point_ *breakPointPo;

typedef struct _break_point_ {
  short arity;
  char name[MAX_SYMB_LEN];
} BreakPoint;

static retCode addBreakPoint(breakPointPo bp);
static logical breakPointHit(char *name, short arity);
static retCode clearBreakPoint(breakPointPo bp);
static retCode parseBreakPoint(char *buffer, long bLen, breakPointPo bp);

retCode g__ins_debug(processPo P, ptrPo a) {
  debugging = interactive = True;
  return Ok;
}

/* waiting for next instruction */
long cmdCounter = 0;

static long cmdCount(char *cmdLine) {
  int64 count = (long) parseInteger(cmdLine, uniStrLen((char *) cmdLine));
  if (count == 0)
    return 1; /* never return 0 */
  else
    return count;
}

static processPo focus = NULL;

static pthread_mutex_t debugMutex = PTHREAD_MUTEX_INITIALIZER;

static void clrCmdLine(char *cmdLine, long len) {
  strMsg(cmdLine, len, "n\n"); /* default to next instruction */
}

static BreakPoint breakPoints[10];
static int breakPointCount = 0;

retCode addBreakPoint(breakPointPo bp) {
  for (int ix = 0; ix < breakPointCount; ix++) {
    if (breakPoints[ix].arity == -1) {
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

logical breakPointHit(char *name, short arity) {
  for (int ix = 0; ix < breakPointCount; ix++) {
    if (breakPoints[ix].arity == arity && uniCmp(breakPoints[ix].name, name) == same)
      return True;
  }
  return False;
}

retCode clearBreakPoint(breakPointPo bp) {
  for (int ix = 0; ix < breakPointCount; ix++) {
    if (breakPoints[ix].arity == bp->arity && uniCmp(breakPoints[ix].name, bp->name) == same) {
      if (ix == breakPointCount - 1) {
        breakPointCount--;
        while (breakPointCount >= 0 && breakPoints[breakPointCount].arity == -1)
          breakPointCount--;
        return Ok;
      } else {
        breakPoints[ix].arity = -1;
        return Ok;
      }
    }
  }

  return Fail;
}

static retCode parseBreakPoint(char *buffer, long bLen, breakPointPo bp) {
  integer b = 0;
  integer ix = 0;

  while (ix < bLen && buffer[ix] == ' ')
    ix++;

  while (ix < bLen) {
    codePoint cp = nextCodePoint(buffer, &ix, bLen);
    switch (cp) {
      case '\n':
      case 0:
        appendCodePoint(bp->name, &b, NumberOf(bp->name), 0);
        bp->arity = 0;
        return Eof;
      case '/': {
        appendCodePoint(bp->name, &b, NumberOf(bp->name), 0);
        integer arity = parseInteger(&buffer[ix], (integer) (bLen - ix));
        bp->arity = (short) arity;
        return Ok;
      }
      default:
        appendCodePoint(bp->name, &b, NumberOf(bp->name), cp);
        continue;
    }
  }
  return Error;
}

retCode breakPoint(processPo P) {
  return Ok;
}

void dC(termPo w) {
  outMsg(logFile, "%w\n", w);
  flushOut();
}

static retCode showConstant(ioPo out, methodPo mtd, int off) {
  return outMsg(out, " %T", nthArg(mtd->pool, off));
}

static void showRegisters(int64 pcCount, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp);

void debug_stop(integer pcCount, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  static char line[256] = {'n', 0};

  static processPo focus = NULL; /* non-null implies only interested in this */
  static integer traceCount = 0;

  if (focus == NULL || focus == p) {
    insWord PCX = *pc;

    disass(pcCount, p, mtd, pc, fp, sp);
    if (!interactive || traceCount > 0) {
      if (traceCount == 0)
        outMsg(logFile, "\n");
      else {
        traceCount--;
        if (traceCount > 0)
          outMsg(logFile, "\n");
      }
      flushFile(logFile);
    }

    while (interactive && traceCount == 0) {
      char *ln = line;
      outMsg(logFile, " => ");
      flushFile(logFile);

      codePoint ch;
      retCode res = inChar(stdIn, &ch);

      if (res == Ok && ch != '\n') {
        do {
          *ln++ = (char) ch;
          res = inChar(stdIn, &ch);
        } while (ch != '\n' && res == Ok);
        *ln++ = '\0';
      }

      switch (line[0]) {
        case ' ':
        case 'n':
          cmdCounter = cmdCount(line + 1);
          tracing = True;
          clrCmdLine(line, NumberOf(line));
          p->waitFor = nextIns;
          break;
        case '\n':
          break;
        case 'f':
          focus = p;
          clrCmdLine(line, NumberOf(line));
          break;
        case 'u':
          focus = NULL;
          clrCmdLine(line, NumberOf(line));
          break;
        case 'q':
          outMsg(logFile, "terminating session");
          exit(0);

        case 't':
          interactive = False;
          break;
        case 'c':
          tracing = False;
          break;
        case 'r':      /* dump the registers */
          showRegisters(pcCount, p, mtd, pc, fp, sp);
          continue;
        case 'l': {    /* dump a local variable */
          logMsg(logFile, "not implemented\n");
          continue;
        }
        case 'e': {    /* dump an environment variable */
          logMsg(logFile, "not implemented\n");
          continue;
        }
        case 'P': {    /* Display all processes */
          logMsg(logFile, "not implemented\n");
          continue;
        }

        case 's':      /* Show a stack trace of this process */
          logMsg(logFile, "not implemented\n");
          continue;

        case '0':
        case '1':
        case '2':
        case '3':
        case '4':
        case '5':
        case '6':
        case '7':
        case '8':
        case '9': {
          traceCount = parseInteger(line, uniStrLen(line));
          continue;
        }

        case 'i': {
          integer off = parseInteger(line + 1, uniStrLen(line + 1));
          integer i;
          insPo pc0 = pc;

          for (i = 0; i < off; i++) {
            pc0 = disass(pcCount + i, p, mtd, pc0, fp, sp);
            outChar(logFile, '\n');
          }

          clrCmdLine(line, NumberOf(line));
          continue;
        }

        default:
          outMsg(logFile, "'n' = step, 'c' = continue, 't' = trace mode, 'q' = stop\n");
          outMsg(logFile, "'<n>' = step <n>\n");
          outMsg(logFile, "'r' = registers, 'l <n>' = local\n");
          outMsg(logFile, "'i'<n> = list n instructions, 's' = stack trace\n");
          outMsg(logFile, "'f' = focus on this process, 'u' = unfocus \n");
          continue;
      }
      return;
    }
  }
}

static void showLine(ioPo out, termPo ln) {
  if (isNormalPo(ln)) {
    normalPo line = C_TERM(ln);
    if (labelCmp(line->lbl, locLbl) == same) {
      outMsg(out, "%T:%T(%T)\n", nthArg(line, 0), nthArg(line, 1), nthArg(line, 4));
      flushFile(out);
      return;
    }
  }
  outMsg(out, "line: %T\n", ln);
}

void debug_line(integer pcCount, processPo p, termPo line) {
  showLine(logFile, line);
}

#define collectI32(pc) (collI32(pc))
#define collI32(pc) hi32 = (uint32)(*(pc)++), lo32 = *(pc)++, ((hi32<<16)|lo32)

insPo disass(integer pcCount, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  int32 hi32, lo32;

  integer offset = (integer) (pc - entryPoint(mtd));

  outMsg(logFile, "0x%x[%d]: %T(%d) ", pc, pcCount, nthArg(codeLits(mtd), 0), offset);

  switch (*pc++) {
#undef instruction

#define show_nOp
#define show_i32 outMsg(logFile," #%d",collectI32(pc))
#define show_arg outMsg(logFile," a[%d]",collectI32(pc))
#define show_lcl outMsg(logFile," l[%d]",collectI32(pc))
#define show_off outMsg(logFile," 0x%x",(collI32(pc)+pc))
#define show_Es outMsg(logFile, " (%U)", getEscape(collectI32(pc))->name)
#define show_lit outMsg(logFile," %T",nthArg(mtd->pool, collectI32(pc)))

#define instruction(Op, A1, Cmt)    \
    case Op:          \
      outMsg(logFile," %s",#Op);    \
      show_##A1;        \
  return pc;

#include "instructions.h"

    default:
      return pc;
  }
}

void showRegisters(int64 pcCount, processPo p, methodPo mtd, insPo pc, framePo fp, ptrPo sp) {
  outMsg(logFile, "p: 0x%x, mtd: %M, pc: 0x%x, fp: 0x%x, sp: 0x%x\n",
         p, mtd, pc, fp, sp);

  integer pcOffset = (integer) (pc - mtd->code);

  normalPo locals = mtd->locals;
  int64 numLocals = termArity(locals);

  for (int32 ix = 0; ix < numLocals; ix++) {
    normalPo lcl = C_TERM(nthArg(locals, ix));

    integer from = integerVal(nthArg(lcl, 0));
    integer to = integerVal(nthArg(lcl, 1));

    if (from <= pcOffset && to > pcOffset) {
      integer off = integerVal(nthArg(lcl, 2));
      termPo var = localVar(fp, off);
      termPo vrName = nthArg(lcl, 4);
      termPo sig = nthArg(lcl, 3);

      outMsg(logFile, "%T [%d]:%T ", vrName, off, sig);
      outMsg(logFile, "\n");
    }
  }
}

static integer insCounts[illegalOp];

void countIns(insWord ins) {
  insCounts[ins]++;
}

#undef instruction
#define instruction(Op, Arg, Cmt) if(insCounts[Op]!=0) outMsg(logFile,"#Op: %d - #Cmt\n",insCounts[Op]);

void dumpInsCount() {
#include "instructions.h"
}
