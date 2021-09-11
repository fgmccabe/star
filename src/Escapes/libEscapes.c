#include "config.h"
#include <ooio.h>
#include <assert.h>
#include "term.h"
#include <globals.h>
#include <chars.h>
#include "engine.h"
#include "libEscapes.h"
#include "signature.h"

static EscapeRec escapes[256];
static int topEsc = 0;

static integer escCount[256];

static int installEscape(char *name, char *sig, libFun fun);

#undef escape
#define escape(Fun, Sig, Cmnt)\
extern ReturnStatus g_##Fun(processPo p,ptrPo tos);\
  installEscape(#Fun,Sig,g_##Fun);

void installEscapes() {
  topEsc = 0;

#include "escapes.h"

#undef escape

}

int installEscape(char *name, char *sig, libFun fun) {
  int escIx = topEsc++;
  escapePo esc = &escapes[escIx];

  esc->name = uniDuplicate(name);
  esc->sig = uniDuplicate(sig);
  esc->fun = fun;
  integer arity;

  funSigArity(sig, &arity);
  esc->arity = arity;
  return escIx;
}

uint32 lookupEscape(char *name) {
  for (int ix = 0; ix < topEsc; ix++) {
    if (uniCmp(name, escapes[ix].name) == same)
      return ix;
  }
  logMsg(logFile,"cannot find escape %s",name);
  syserr("could not find escape");
  return -1;
}

escapePo getEscape(uint32 escNo) {
  assert(escNo >= 0 && escNo < topEsc);
  return &escapes[escNo];
}

ReturnStatus rtnStatus(processPo p, retCode ret, char *msg) {
  ReturnStatus rtn = {.ret = ret};

  switch (ret) {
    case Ok:
      rtn.result = okEnum;
      return rtn;
    case Fail:
      rtn.result = failEnum;
      return rtn;
    case Eof:
      rtn.result = eofEnum;
      return rtn;
    case Error: {
      heapPo H = processHeap(p);
      normalPo err = allocateStruct(H, (labelPo) errorLbl);
      int root = gcAddRoot(H, (ptrPo) (&err));
      setArg(err, 0, (termPo) allocateChars(H, msg, uniStrLen(msg)));
      gcReleaseRoot(H, root);
      rtn.result = (termPo) err;
      return rtn;
    }
    default:
      return rtnStatus(p, Error, "cannot handle return");
  }
}

#ifdef TRACESTATS

void recordEscape(integer escNo) {
  assert(escNo >= 0 && escNo < NumberOf(escCount));

  escCount[escNo]++;
}

static void dumpEsc(escapePo esc, ioPo out, integer escNo) {
  if (escCount[escNo] > 0)
    outMsg(out, "%s:%d\n", esc->name, escCount[escNo]);
}

void dumpEscapes(ioPo out) {
  outMsg(out, "escapes executed\n");
  for (integer ix = 0; ix < NumberOf(escCount); ix++)
    dumpEsc(&escapes[ix], out, ix);
}

#endif
