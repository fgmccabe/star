/*
 * Handle compilation of expressions
 */

#include "compiler.h"
#include "dict.h"
#include "utils.h"
#include "compile.h"
#include "meta.h"
#include "escapes.h"
#include "codegen.h"

retCode compileExp(sxPo exp,sxPo *expected,
		   uniChar *path,
		   dictPo dict,dictPo outer,
		   exitPo exit,mtdPo mtd,
		   contFun cont,void *cl)
{
  locationPo loc = sxLoc(exp);
  assemPo code = methodCode(mtd);

  if(sxIsIden(exp)){
    uniChar *vrName = sxIden(exp);

    if(isConstructor(vrName,dict))
      return compileConstructor(exp,expected,
				path,dict,outer,exit,mtd,cont,cl);
    else if(isLibVar(vrName))
      return compileLibVar(loc,vrName,expected,
			   path,dict,outer,exit,mtd,cont,cl);
    else{
      varInfoPo var = varReference(vrName,dict);

      if(var==Null){
	reportError(loc,"variable: %A not declared",exp);
	return Error;
      }
      else if(checkType(var->type,expected,loc)!=Ok){
	reportError(loc,"variable: %A:%T not consistent with %T",
		    exp,var->type,*expected);
	return Error;
      }
      else
	return cont(loc,var,cl,code);
    }
  }
  else if(sxIsChar(exp)){
    VarInfoRec src = {.loc=loc,.where=literal,.kind=rawChar,.l.ix=sxChar(exp)};
    return cont(loc,&src,cl,code);
  }
  else if(sxIsStr(exp)){
    lPo lit = defineLiteralString(mtd,sxText(exp));
    VarInfoRec src = { .loc=loc, .where=label, .kind=general, .l.lit=lit};
    return cont(loc,&src,cl,code);
  }
  else if(sxIsInt(exp)){
    VarInfoRec src = {.loc=loc,.where=literal,.kind=rawInt,.l.ix=sxInt(exp)};
    return cont(loc,&src,cl,code);
  }
  else if(sxIsLong(exp)){
    VarInfoRec src = {.loc=loc,.where=literal,.kind=rawLong,.l.ix=sxLong(exp)};
    return cont(loc,&src,cl,code);
  }
  else if(sxIsFloat(exp)){
    VarInfoRec src = {.loc=loc,.where=literal,.kind=rawFloat,.l.d=sxFloat(exp)};
    return cont(loc,&src,cl,code);
  }
  else if(sxIsConstructor(exp))
    return compileConstructor(exp,expected,path,dict,outer,exit,mtd,cont,cl);
  else if(sxIsCall(exp)){
    uniChar *op = sxCallOp(exp);
    lxPo args = sxCallArgs(exp);

    if(isEscape(op))
      return compileEscape(loc,op,expected,path,dict,outer,exit,mtd,args,cont,cl);
    else if(isLibFun(op))
      return compileCCall(exp,expected,path,dict,outer,exit,mtd,cont,cl);
    else{
      varInfoPo var = varReference(op,dict);
      if(var==Null){
	reportError(loc,"function %U not declared",op);
	return Error;
      }
      else{
	int stkDepth = localDepth(dict);
	int argDepth = -ALIGN(-stkDepth+argSize(var->type),16)-POINTER_SIZE;

	AStartCall(code,argDepth);
	tryRet(compileArgs(args,arrowArgTypes(var->type),argDepth,
			   path,dict,outer,exit,mtd));

	switch(var->where){
	case basedVar:
	  ACallX(code,var->base,var->l.off);
	  break;
	case label:
	  ACallLbl(code,var->l.lit);
	  break;
	default:
	  reportError(loc,"variable %V not valid for execution",var);
	  return Error;
	}

	gcCallSite(mtd,dict);	/* we build code that marks active vars */

	sxPo resType = arrowResType(var->type);
	sourceKind kind = typeRep(resType);

	switch(kind){
	case rawFloat:{
	  VarInfoRec res = {.loc=loc,.where=registr,.kind=rawFloat,.l.fpReg = FPR0};
	  return cont(loc,&res,cl,code);
	}
	default:{
	  VarInfoRec res = {.loc=loc,.where=registr,.kind=kind,.l.reg = R0 };
	  return cont(loc,&res,cl,code);
	}
	}
      }
    }
  }
  else if(sxIsArithExp(exp))
    return compileArithmetic(exp,expected,path,dict,outer,exit,mtd,cont,cl);
  else if(sxIsCast(exp)){
    sxPo castType = sxCastType(exp);
    VarInfoRec tgt = {.loc=sxLoc(exp), .where=registr,.kind=typeRep(castType)};

    if(tgt.kind==rawFloat)
      tgt.l.fpReg = FPR0;
    else
      tgt.l.reg = R0;

    tryRet(checkType(castType,expected,loc));

    lPo nxLbl = newLbl(code,genSym(".Nx"));
    Combo combo = {.cont1=typeConvert,.cl1=&tgt,.cont2=jumpCont,.cl2=nxLbl};
    sxPo expType = Null;
    compileExp(sxCastExp(exp),&expType,
	       path,dict,outer,exit,mtd,comboCont,&combo);
    jumpTarget(code,nxLbl);
    return cont(loc,&tgt,cl,code);
  }
  else if(sxIsSwitch(exp))
    return compileSwitch(exp,expected,path,dict,outer,exit,mtd,compileExp,cont,cl);
  else if(sxIsLet(exp))
    return compileTheta(sxLetDefs(exp),path,dict,outer,compileExp,expected,
			sxLetBound(exp),
			exit,mtd,cont,cl);
  else if(sxIsValof(exp)){
    lPo exitLabel = newLbl(code,genSym(".V"));

    if(isJumpCont(cont,cl)){
      ExitLabel vExit = {.name=ValofName, .cont=cont, .cl=cl, .outer=exit};
      return compileAction(sxValofAction(exp),expected,
			   path,dict,outer,&vExit,mtd,errorCont,sxLoc(exp));
    }
    else{
      Combo combo = { .cont1=cont,.cont2=jumpCont,.cl1=cl,.cl2=exitLabel };
      ExitLabel vExit = {.name=ValofName, .cont=comboCont,.cl=&combo,.outer=exit};
      retCode ret = compileAction(sxValofAction(exp),expected,
				  path,dict,outer,&vExit,mtd,
				  errorCont,sxLoc(exp));
      defineLbl(code,exitLabel);
      return ret;
    }
  }
  else{
    reportError(loc,"cannot handle %A",exp);
    return Error;
  }
}

static long countLocalsInExpCases(lxPo cases);
static long countLocalsInExps(lxPo exps);

long countLocalsInExp(sxPo exp)
{
  if(sxIsValof(exp))
    return countLocalsInAct(sxValofAction(exp));
  else if(sxIsSwitch(exp)){
    long count = countLocalsInExp(sxSwitchSel(exp));
    lxPo cases = sxSwitchCases(exp);

    return count+countLocalsInExpCases(cases);
  }
  else if(sxIsLet(exp))
    return countLocalsInDefs(sxLetDefs(exp))+countLocalsInExp(sxLetBound(exp));
  else if(sxIsCall(exp))
    return countLocalsInExps(sxCallArgs(exp));
  else if(sxIsConstructor(exp))
    return countLocalsInExps(sxConstructorArgs(exp));
  else if(sxIsCast(exp))
    return countLocalsInExp(sxCastExp(exp));
  else
    return 0;
}

static long countLocalsInExps(lxPo exps)
{
  long count = 0;
  for(int ix=0;ix<sxLength(exps);ix++)
    count+=countLocalsInExp(sxEl(exps,ix));
  return count;
}

static long countLocalsInExpCases(lxPo cases)
{
  long caseCount = sxLength(cases);

  long max = 0;

  for(int ix=0;ix<caseCount;ix++){
    sxPo cse = sxEl(cases,ix);
    long cCount;

    if(sxIsCaseRule(cse))
      cCount = countLocalsInPtn(sxCasePtn(cse))+
	countLocalsInExp(sxCaseBody(cse));
    else if(sxIsDefaultRule(cse))
      cCount = countLocalsInExp(sxDefltBody(cse));

    if(cCount>max)
      max = cCount;
  }
  return max;
}

long countLocalsInPtn(sxPo ptn)
{
  if(sxIsConstructor(ptn))
    return countLocalsInPtnArgs(sxConstructorArgs(ptn));
  else if(sxIsCast(ptn)){
    if(sxIden(sxCastExp(ptn))!=ANONYMOUS)
      return typeSize(sxCastType(ptn));
    else
      return 0;
  }
  else
    return 0;
}

long countLocalsInPtnArgs(lxPo args)
{
  long count = 0;
  long arity = sxLength(args);
  for(int ix=0;ix<arity;ix++)
    count+=countLocalsInPtn(sxEl(args,ix));
  return count;
}

long argSize(sxPo type)
{
  lxPo argTypes = arrowArgTypes(type);
  long arity = sxLength(argTypes);
  long size = 0;
  for(int ix=0;ix<arity;ix++){
    sxPo aType = sxEl(argTypes,ix);
    size += typeSize(aType);
  }
  return size;
}

retCode compileArgs(lxPo args,lxPo argTypes,
		    int depth,uniChar *path,
		    dictPo dict,dictPo outer,
		    exitPo exit,mtdPo mtd)
{
  long arity = sxLength(args);
  int offset = depth;
  retCode res = Ok;

  for(int ix=0;res==Ok && ix<arity;ix++){
    sxPo arg = sxEl(args,ix);
    locationPo loc = sxLoc(arg);
    sxPo argType = sxEl(argTypes,ix);
    sourceKind mcType = typeRep(argType);
    VarInfoRec tgt = {.loc=loc,.where=basedVar,.base=FP,
		      .kind=mcType,.l.off=offset};

    res = compileExp(arg,&argType,path,dict,outer,exit,mtd,assignVar,&tgt);
    offset+=sourceSize(mcType);
  }
  return res;
}

