/*
 * Pattern code
 */
#include "compiler.h"

#include "utils.h"
#include "compile.h"
#include "meta.h"
#include "codegen.h"

/* Generate the matching code for a constructor. We access and declare all
 * arguments that are not anonymous. We do not enter a constructor's pattern
 * code unless we know it will match. (That is done by the case analysis). 
 */

retCode genPtnCode(sxPo con,uniChar *path,dictPo dict,mtdCxtPo mtd)
{
  assemPo code = methodCode(mtd);

  if(sxIsIden(con))
    return Ok;
  else if(sxIsConstructor(con)){
    lxPo args = sxConstructorArgs(con);
    long arity = sxLength(args);

    retCode ret = Ok;
    long conOffset = POINTER_SIZE;

    for(int ix=0;ret==Ok && ix<arity;ix++){
      sxPo arg = sxEl(args,ix);
      locationPo loc = sxLoc(arg);
      if(sxIsCast(arg) && sxIsIden(sxCastExp(arg))){ /* name:type */
	sxPo argType = sxCastType(arg);
	uniChar *var = sxIden(sxCastExp(arg));
	sourceKind kind = typeRep(argType);

	if(uniCmp(var,ANONYMOUS)!=0){
	  varInfoPo v = reserve(loc,var,argType,readOnly,True,kind,dict);
	  VarInfoRec tSrc = { .where=basedVar,.base=TRM,
			      .kind=kind, .l.off=conOffset};
	  tryRet(assignVar(loc,&tSrc,v,code));
	  declareVar(v,dict);
	}
	conOffset+=sourceSize(kind);
      }
      else
	reportError(loc,"invalid pattern: %A, expecting (name:type)",arg);
    }
    return ret;
  }
  else{
    reportError(sxLoc(con),"cannot generate code for %A",con);
    return Error;
  }
}

retCode genPtnArgs(lxPo args,uniChar *path,dictPo dict,mtdCxtPo mtd)
{
  assemPo code = methodCode(mtd);
  long arity = sxLength(args);

  retCode ret = Ok;
  long conOffset = POINTER_SIZE;

  for(int ix=0;ret==Ok && ix<arity;ix++){
    sxPo arg = sxEl(args,ix);
    locationPo loc = sxLoc(arg);
    if(sxIsCast(arg) && sxIsIden(sxCastExp(arg))){
      sxPo argType = sxCastType(arg);
      uniChar *var = sxIden(sxCastExp(arg));
      sourceKind kind = typeRep(argType);
    
      if(uniCmp(var,ANONYMOUS)!=0){
	varInfoPo v = reserve(loc,var,argType,readOnly,True,kind,dict);
	VarInfoRec tSrc = { .where=basedVar,.base=TRM,
			    .kind=kind, .l.off=conOffset};
	tryRet(assignVar(loc,&tSrc,v,code));
	declareVar(v,dict);
      }
      conOffset+=sourceSize(kind);
    }
    else
      reportError(loc,"invalid pattern: %A, expecting (name:type)",arg);
  }
  return ret;
}

