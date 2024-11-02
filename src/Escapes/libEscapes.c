#include "config.h"
#include <ooio.h>
#include <assert.h>
#include "term.h"
#include <globals.h>
#include "engine.h"
#include "libEscapes.h"
#include "signature.h"
#include "quick.h"

static EscapeRec escapes[256];

static int topEsc = 0;

static integer escCount[256];

static int installEscape(EscapeCode code, char *name, char *sig, libFun fun);

#undef escape
#define escape(Fun, Sig, Cmnt)\
extern ReturnStatus g_##Fun(heapPo h);\
  installEscape(Esc##Fun,#Fun,Sig,g_##Fun);

void installEscapes() {
  topEsc = 0;

#include "escapes.h"

#undef escape

}

int installEscape(EscapeCode code, char *name, char *sig, libFun fun) {
  int escIx = topEsc++;
  escapePo esc = &escapes[code];

  esc->name = uniDuplicate(name);
  esc->sig = uniDuplicate(sig);
  esc->fun = fun;
  int32 arity;

  funSigArity(sig, &arity);
  esc->arity = arity;
  assert(arity <= MAX_ESCAPE_ARITY);

  return escIx;
}

uint32 lookupEscape(char *name) {
  for (int ix = 0; ix < topEsc; ix++) {
    if (uniCmp(name, escapes[ix].name) == same)
      return ix;
  }
  logMsg(logFile, "cannot find escape %s", name);
  syserr("could not find escape");
  return -1;
}

escapePo getEscape(uint32 escNo) {
  assert(escNo >= 0 && escNo < topEsc);
  return &escapes[escNo];
}

void recordEscape(integer escNo) {
  assert(escNo >= 0 && escNo < NumberOf(escCount));

  escCount[escNo]++;
}

static void dumpEsc(escapePo esc, ioPo out, integer escNo) {
  if (escCount[escNo] > 0)
    outMsg(out, "%s:%d\n", esc->name, escCount[escNo]);
}

static comparison cmpCount(integer i, integer j, void *cl) {
  integer *indices = (integer *) cl;

  integer iCount = escCount[indices[i]];
  integer jCount = escCount[indices[j]];

  if (iCount < jCount)
    return smaller;
  else if (iCount == jCount)
    return same;
  else
    return bigger;
}

static retCode swapIndex(integer i, integer j, void *cl) {
  integer *indices = (integer *) cl;
  integer w = indices[i];
  indices[i] = indices[j];
  indices[j] = w;
  return Ok;
}

void dumpEscapes(ioPo out) {
  outMsg(out, "escapes executed\n");

  integer indices[NumberOf(escCount)];
  for (int ix = 0; ix < NumberOf(escCount); ix++)
    indices[ix] = ix;

  // Sort them by frequency
  quick(0, NumberOf(escCount) - 1, cmpCount, swapIndex, (void *) indices);

  for (integer ix = 0; ix < NumberOf(escCount); ix++) {
    integer escNo = indices[ix];
    dumpEsc(&escapes[escNo], out, escNo);
  }
}

char *escapeName(escapePo esc) {
  return esc->name;
}

int32 escapeArity(escapePo esc) {
  return esc->arity;
}

libFun escapeFun(escapePo esc) {
  return esc->fun;
}

