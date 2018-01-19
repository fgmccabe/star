/*
 * top-level run-time functions
 */
#include "cafe.h"
#include <stdlib.h>
#include <assert.h>

#include "engineP.h"
#include "decode.h"

static poolPo prPool;     /* pool of processes */
static char *UMAIN = "_main";

void initEngine() {
  prPool = newPool(sizeof(ProcessRec), 32);
}

int loadAndGo(char *boot, int argc, char *args[]) {
  ioPo in = openInFile(boot, rawEncoding);

  if (in != Null) {
    retCode ret = skipShellPreamble(in);

    hashPo pkg = decodePkg(in,currHeap);
    if (pkg != Null) {
      methodPo umain = (methodPo) hashGet(pkg, UMAIN);
      if (umain != Null) {
        processPo p = newProcess(umain);
        integer reslt = run(p, currHeap);
#ifdef TRACEEXEC
        if (tracing)
          outMsg(logFile, "tos = %ld\n", reslt);
#endif
        return 0;
      } else {
        syserr("no _main");
        return 1;
      }
    } else {
      syserr("cannot load boot file");
      return 99;
    }
  } else {
    syserr("cannot open boot file");
    return 100;
  }
}

static uint16 haltCode[] = {Halt};

processPo newProcess(methodPo cl) {
  processPo P = (processPo) allocPool(prPool);

  P->prog = cl;
  P->pc = entryPoint(cl);
  P->stackBase = (termPo) malloc(sizeof(integer) * initStackSize);
  P->stackLimit = &P->stackBase[initStackSize];

  P->fp = (framePo) P->stackLimit;

  // cap the stack with a halting stop.

  ptrPo sp = (ptrPo) P->fp;
  *--sp = (termPo) cl;
  *--sp = (termPo) &haltCode[0];

  P->sp = sp;

  return P;
}
