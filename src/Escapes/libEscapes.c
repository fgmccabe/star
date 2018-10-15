#include "config.h"
#include <ooio.h>
#include <assert.h>
#include "term.h"
#include <globals.h>
#include <str.h>
#include <stdlib.h>
#include "engine.h"
#include "libEscapes.h"
#include "signature.h"

static EscapeRec escapes[256];
static int topEsc = 0;

static int installEscape(char *name, char *sig, libFun fun);

#undef escape
#define escape(Fun, Visible, Sys, Sig, Cmnt)\
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

int32 lookupEscape(char *name) {
  for (int ix = 0; ix < topEsc; ix++) {
    if (uniCmp(name, escapes[ix].name) == same)
      return ix;
  }
  syserr("could not find escape");
  return -1;
}

escapePo getEscape(int32 escNo) {
  assert(escNo>=0 && escNo < topEsc);
  return &escapes[escNo];
}

ReturnStatus rtnStatus(processPo p, retCode ret, char *msg) {
  ReturnStatus rtn = {.ret = ret};

  switch (ret) {
    case Ok:
      rtn.result = okEnum;
      return rtn;
    case Fail:
      return rtn;
    case Eof:
      rtn.result = eofEnum;
      return rtn;
    case Error: {
      heapPo H = processHeap(p);
      normalPo err = allocateStruct(H, (labelPo) errorLbl);
      int root = gcAddRoot(H, (ptrPo) (&err));
      setArg(err, 0, (termPo) allocateString(H, msg, uniStrLen(msg)));
      gcReleaseRoot(H, root);
      rtn.result = (termPo) err;
      return rtn;
    }
    default:
      return rtnStatus(p, Error, "cannot handle return");
  }
}
