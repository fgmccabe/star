/*
 * top-level run-time functions
 */
#include "cafe.h"
#include <stdlib.h>
#include <assert.h>

#include "engine.h"
#include "escape.h"
#include "decode.h"

static poolPo prPool;     /* pool of processes */
static char UMAIN[] = {'_', 'm', 'a', 'i', 'n', 0};

void initEngine() {
  prPool = newPool(sizeof(ProcessRec), 32);
  initEscapes();
}

int loadAndGo(char *boot, int argc, char *args[]) {
  ioPo in = openInFile(boot, rawEncoding);

  if (in != Null) {
    char ch = inCh(in);    /* We skip over #! */

    if (ch == '#') {      /* look for standard #!/.... header */
      if ((ch = inCh(in)) == '!') {
        while ((ch = inCh(in)) != uniEOF && ch != '\n');             /* consume the interpreter statement */
      } else {
        unGetChar(in, ch);
        unGetChar(in, '#');
      }
    } else
      unGetChar(in, ch);

    hashPo pkg = decodePkg(in);
    if (pkg != Null) {
      methodPo umain = (methodPo) hashGet(pkg, UMAIN);
      if (umain != Null) {
        closurePo mainCl = allocate(currHeap, umain);
        assert(mainCl != Null);
        processPo p = newProcess(mainCl);
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

processPo newProcess(closurePo cl) {
  processPo P = (processPo) allocPool(prPool);

  P->prog = cl;
  P->pc = entryPoint(cl);
  P->stackBase = (termPo) malloc(sizeof(integer) * initStackSize);
  P->stackLimit = &P->stackBase[initStackSize];

  P->fp = (framePo) P->stackLimit;

  // cap the stack with a halting stop.

  termPo sp = (termPo) P->fp;
  *--sp = (integer) cl;
  *--sp = (integer) &haltCode[0];

  P->sp = sp;

  return P;
}
