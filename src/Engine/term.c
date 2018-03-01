//
// Created by Francis McCabe on 1/15/18.
//

#include "codeP.h"
#include <assert.h>
#include "formioP.h"

SpecialClass SpecialClss = {
  .clss = Null,
  .sizeFun = Null,
  .copyFun = Null,
  .scanFun = Null
};

clssPo specialClass = (clssPo) &SpecialClss;

extern normalPo C_TERM(termPo t) {
  assert(hasClass(t, normalClass));
  return (normalPo) t;
}

termPo nthArg(normalPo term, int64 nth) {
  return term->args[nth];
}

void setArg(normalPo term, int64 ix, termPo arg) {
  assert(ix >= 0 && ix < termArity(term));
  term->args[ix] = arg;
}

static retCode showTerm(ioPo f, void *data, long depth, long precision, logical alt) {
  return dispTerm(f, (termPo) data, depth, alt);
}

void initTerm() {
  installMsgProc('T', showTerm);
}
