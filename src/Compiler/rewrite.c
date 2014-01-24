/*
 * Rewriting utility
 */

#include "compiler.h"

#include "utils.h"
#include "meta.h"
#include "rewrite.h"

static lxPo rewriteSeq(lxPo l,rewriteFun rewrite,void *cl);

sxPo rewrite(sxPo sx,rewriteFun rew,void *cl)
{
  switch(rew(&sx,cl)){
  case Error:
  default:
    reportError(sxLoc(sx),"could not rewrite %A",sx);
    return sx;
  case Ok:
    return sx;
  case Fail:
    // rewriter did not apply
    if(sxIsApply(sx)){
      sxPo op = rewrite(sxOp(sx),rew,cl);
      lxPo a = rewriteSeq(sxArgs(sx),rew,cl);
      if(op!=sxOp(sx) || a!=sxArgs(sx))
	return mApply(sxLoc(sx),op,a);
      else
	return sx;
    }
    else
      return sx;
  }
}

lxPo rewriteSeq(lxPo l,rewriteFun rew,void *cl)
{
  if(l==nil)
    return l;
  else{
    sxPo h = rewrite(sxHead(l),rew,cl);
    lxPo t = rewriteSeq(sxTail(l),rew,cl);
    if(h!=sxHead(l) || t!=sxTail(l))
      return mCons(h,t);
    else
      return l;
  }
}






