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
  .scanFun = Null,
  .dispFun = Null
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

retCode dispTerm(ioPo out, termPo t, long depth, logical alt){
  clssPo clss = t->clss;
  if(isSpecialClass(clss)){
    specialClassPo spec = (specialClassPo)clss;
    return spec->dispFun(out,t,depth,alt);
  } else{
    normalPo nml = C_TERM(t);
    labelPo lbl = nml->lbl;
    retCode ret = outStr(out,lbl->name);
    if(ret==Ok)
      ret = outChar(out,'(');
    if(depth>0){
      char *sep = "";
      integer ar = lbl->arity;
      for(integer ix=0;ix<ar && ret==Ok;ix++){
        ret = outStr(out,sep);
        sep = ", ";
        if(ret == Ok)
          ret = dispTerm(out,nthArg(nml,ix),depth-1,alt);
      }
    }
    else
      ret = outStr(out,"...");
    if(ret==Ok)
      ret = outChar(out,')');
    return ret;
  }
}
