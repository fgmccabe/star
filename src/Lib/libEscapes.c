#include "config.h"
#include <ooio.h>
#include <assert.h>
#include "term.h"
#include <globals.h>
#include <str.h>
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

escapePo getEscape(int32 escNo) {
  assert(escNo < topEsc);
  return &escapes[escNo];
}

ReturnStatus rtnStatus(processPo p, retCode ret, char *msg) {
  ReturnStatus rtn = {.ret = ret};

  switch (ret) {
    case Ok:
      rtn.rslt = okEnum;
      return rtn;
    case Fail:
      return rtn;
    case Eof:
      rtn.rslt = eofEnum;
      return rtn;
    case Error: {
      normalPo err = allocateStruct(processHeap(p), (labelPo) errorLbl);
      int root = gcAddRoot((ptrPo) (&err));
      setArg(err, 0, allocateString(processHeap(p), msg, uniStrLen(msg)));
      gcReleaseRoot(root);
      rtn.rslt = (termPo) err;
      return rtn;
    }
    default:
      return rtnStatus(p, Error, "cannot handle return");
  }
}




