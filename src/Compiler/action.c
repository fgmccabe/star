/*
 * Handle compilation of actions
 */

#include "compiler.h"
#include "dict.h"
#include "utils.h"
#include "meta.h"
#include "compile.h"
#include "escapes.h"
#include "codegen.h"

static retCode compileBlock(lxPo actions,sxPo *expected,uniChar *path,
			    dictPo dict,dictPo outer,
			    exitPo exit, mtdCxtPo mtd, 
			    contFun cont,void *cl);

static retCode compileVarDeclaration(sxPo var,rwMode access,
				     sxPo value,uniChar *path,
				     dictPo dict,dictPo outer,
				     exitPo exit,mtdCxtPo mtd);

static retCode thrower(locationPo loc,varInfoPo src,void *cl,assemPo code);
static retCode unwinder(locationPo loc,varInfoPo src,void *cl,assemPo code);
static retCode asserter(locationPo loc,varInfoPo src,void *cl,assemPo code);

retCode compileAction(sxPo act,sxPo *expected,uniChar *path,
		      dictPo dict,dictPo outer,exitPo exit,
		      mtdCxtPo mtd, contFun cont,void *cl)
{
  locationPo loc = sxLoc(act);
  assemPo code = methodCode(mtd);

  if(sxIsIdentifier(act,Nothing))
    return cont(loc,Null,cl,code);	/* Do nothing, invoke continuation */
  else if(sxIsBlock(act))
    return compileBlock(sxBlockContent(act),expected,
			path,dict,outer,exit,mtd,cont,cl);
  else if(sxIsCall(act)){
    uniChar *op = sxCallOp(act);
    sxPo resultType = Null;
    lxPo args = sxCallArgs(act);

    if(isEscape(op))
      return compileEscape(loc,op,&resultType,
			   path,dict,outer,exit,mtd,args,cont,cl);
    else if(isLibFun(op))
      return compileCCall(act,&resultType,path,dict,outer,exit,mtd,cont,cl);
    else{
      varInfoPo var = varReference(op,dict);
      if(var==Null){
	reportError(loc,"%U not declared",op);
	return Error;
      }
      else{
	int stkDepth = localDepth(dict);
	int argDepth = -ALIGN(-stkDepth+argSize(var->type),16)-POINTER_SIZE;

	AStartCall(code,argDepth);
	tryRet(compileArgs(args,arrowArgTypes(var->type),
			   argDepth,path,dict,outer,exit,mtd));

	// Call the function itself. It contains the environment
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
	
	gcCallSite(mtd,dict);
	return cont(loc,Null,cl,code);
      }
    }
  }
  else if(sxIsIsDeclaration(act)){
    compileVarDeclaration(sxDeclLval(act),
			  readOnly,sxDeclValue(act),
			  path,dict,outer,exit,mtd);
    return cont(loc,Null,cl,code);
  }
  else if(sxIsVarDeclaration(act)){
    compileVarDeclaration(sxDeclLval(act),readWrite,sxDeclValue(act),
			  path,dict,outer,exit,mtd);
    return cont(loc,Null,cl,code);
  }
  else if(sxIsAssignment(act)){
    sxPo var = sxAsgnLVal(act);
    varInfoPo vr = varReference(sxIden(var),dict);
    if(vr==Null){
      reportError(sxLoc(var),"variable %A not declared",var);
      return Error;
    }
    else{
      switch(vr->access){
      case readOnly:
	reportError(sxLoc(var),"not permitted to assign to variable %A",var);
	return Error;
      case readWrite:
	compileExp(sxAsgnRVal(act),&vr->type,path,dict,outer,
		   exit,mtd,assignVar,vr);
	break;
      }
    }
    return cont(loc,Null,cl,code);
  }
  else if(sxIsConditional(act)){
    sxPo tst = sxConditionalTest(act);
    sxPo th = sxConditionalThen(act);
    sxPo el = sxConditionalElse(act);

    lPo elLbl = newLbl(code,genSym(".L"));
    lPo thLbl = newLbl(code,genSym(".T"));

    tryRet(compileCondition(tst,path,dict,outer,onFailure,elLbl,exit,mtd));
    defineLbl(code,thLbl);

    tryRet(compileAction(th,expected,path,dict,outer,exit,mtd,cont,cl));

    defineLbl(code,elLbl);
    return compileAction(el,expected,path,dict,outer,exit,mtd,cont,cl);
  }
  else if(sxIsLabeled(act)){
    lPo exitLbl = newLbl(code,genSym(".A"));
    ExitLabel labeled = { .name=sxLabeledLabel(act), 
			  .cont=jumpCont, .cl=exitLbl,
			  .outer=exit };
    retCode ret = compileAction(sxLabeledBody(act),expected,
				path,dict,outer,&labeled,mtd,cont,cl);
    defineLbl(code,exitLbl);
    return ret;
  }
  else if(sxIsLeaveAction(act)){
    exitPo leave = exitLabel(exit,sxLeaveLabel(act));
    if(leave==Null){
      reportError(loc,"no outer label %U in scope",sxLeaveLabel(act));
      return Error;
    }
    else
      return leave->cont(loc,Null,leave->cl,code);
  }
  else if(sxIsGotoAction(act)){
    exitPo jump = exitLabel(exit,sxGotoLabel(act));
    if(jump==Null){
      reportError(loc,"no label %U in scope",sxGotoLabel(act));
      return Error;
    }
    else
      return jump->cont(loc,Null,jump->cl,code);
  }
  else if(sxIsSwitch(act))
    return compileSwitch(act,expected,
			 path,dict,outer,exit,mtd,compileAction,cont,cl);
  else if(sxIsValis(act)){
    exitPo valis = exitLabel(exit,ValofName);
    if(valis==Null){
      reportError(loc,"valis not in valof expression");
      return Error;
    }
    else
      return compileExp(sxValisExp(act),expected,
			path,dict,outer,exit,mtd,
			valis->cont,valis->cl);
  }
  else if(sxIsWhile(act)){
    lPo loop = newLbl(code,genSym(".O"));
    lPo tst = newLbl(code,genSym(".L"));
    AJmp(code,tst);
    defineLbl(code,loop);
    tryRet(compileAction(sxWhileBody(act),expected,
			 path,dict,outer,exit,mtd,jumpCont,tst));
    jumpTarget(code,tst);
    tryRet(compileCondition(sxWhileTest(act),path,dict,outer,
				 onSuccess,loop,exit,mtd));
    return cont(loc,Null,cl,code);
  }
  else if(sxIsLoopAction(act)){
    lPo loop = currLbl(code,genSym(".L"));
    return compileAction(sxLoopBody(act),expected,
			 path,dict,outer,exit,mtd,jumpCont,loop);
  }
  else if(sxIsCatch(act))
    return compileCatch(act,expected,path,dict,outer,exit,mtd,cont,cl);
  else if(sxIsThrowAction(act)){
    exitPo catch = exitLabel(exit,CatchName);
    sxPo throwType = Null;
    if(catch==Null)
      return compileExp(sxThrowExp(act),&throwType,
			path,dict,outer,exit,mtd,unwinder,Null);
    else
      return compileExp(sxThrowExp(act),&throwType,
			path,dict,outer,exit,mtd,thrower,catch->cl);
  }
  else if(sxIsAssert(act)){
    sxPo tst = sxAssertCond(act);

    lPo exLbl = newLbl(code,genSym(".S"));

    tryRet(compileCondition(tst,path,dict,outer,onSuccess,exLbl,exit,mtd));

    VarInfoRec src = {.loc=loc,.where=literal,.kind=rawInt,.l.ix=99};

    tryRet(asserter(loc,&src,cl,code));

    defineLbl(code,exLbl);

    return cont(loc,Null,cl,code);
  }
  else if(sxIsLet(act))
    return compileTheta(sxLetDefs(act),path,dict,outer,
			compileAction,expected,sxLetBound(act),
			exit,mtd,cont,cl);
  else{
    reportError(loc,"cannot compile action: %A",act);
    return Error;
  }
}

retCode compileBlock(lxPo actions, sxPo *expected,uniChar *path,
		     dictPo dict,dictPo outer, exitPo exit,
		     mtdCxtPo mtd, contFun cont,void *cl)
{
  int stCount = sxLength(actions);
  exitPo blockExits = exit;
  dictStatePo blockState = dictState(dict);
  assemPo code = methodCode(mtd);

  for(int ix=0;ix<stCount;ix++){
    sxPo act = sxEl(actions,ix);

    if(sxIsLabeled(act)){
      exitPo lbled = (exitPo)malloc(sizeof(ExitLabel));
      lbled->outer = blockExits;
      lbled->cl = newLbl(code,genUSym(sxLabeledLabel(act)));
      lbled->cont = jumpCont;
      lbled->name = sxLabeledLabel(act);
      blockExits = lbled;
    }
  }

  retCode res = Ok;

  for(int ix=0;res==Ok && ix<stCount;ix++){
    sxPo act = sxEl(actions,ix);
    if(sxIsLabeled(act)){
      exitPo xx = exitLabel(blockExits,sxLabeledLabel(act));
      defineLbl(code,(lPo)xx->cl);
    }
    if(ix<stCount-1){ // We set up a continuation to the next action
      lPo nextLabel = newLbl(code,genSym("."));
      res = compileAction(act,expected,path,dict,outer,blockExits,mtd,
			  jumpCont,nextLabel);
      jumpTarget(code,nextLabel);
    }
    else
      res = compileAction(act,expected,path,dict,outer,
			  blockExits,mtd,cont,cl);
  }

  while(blockExits!=exit){
    exitPo outer = blockExits->outer;
    free(blockExits);
    blockExits=outer;
  }
  
  resetDict(dict,blockState);
  return res;
}

retCode compileVarDeclaration(sxPo var,rwMode access,sxPo value,uniChar *path,
			      dictPo dict,dictPo outer,
			      exitPo exit,mtdCxtPo mtd)
{
  locationPo loc = sxLoc(var);
  if(sxIsCast(var) && sxIsIden(sxCastExp(var))){
    uniChar *vrName = sxIden(sxCastExp(var));
    sxPo vrType = sxCastType(var);
    sourceKind kind = typeRep(vrType);
    varInfoPo info = reserve(loc,vrName,vrType,access,True,kind,dict);

    retCode ret = compileExp(value,&info->type,
			     path,dict,outer,exit,mtd,assignVar,info);
    if(ret==Ok)
      declareVar(info,dict);
  
    return ret;
  }
  else{
    reportError(loc,"invalid variable declaration: %A",var);
    return Error;
  }
}

retCode thrower(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  Register T = R0;
  lPo catcher = (lPo)cl;
  tryRet(loadReg(loc,src,&T,code));
  AJmp(code,catcher);
  return Ok;
}

retCode unwinder(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  Register T = R0;
  tryRet(loadReg(loc,src,&T,code));
  AUnwind(code,T);
  return Ok;
}

retCode asserter(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  Register T = R0;
  tryRet(loadReg(loc,src,&T,code));
  AUnwind(code,T);
  return Ok;
}


static long countLocalsInActionCases(lxPo cases);
static long countLocalsInActionCase(sxPo cse);

long countLocalsInAct(sxPo action)
{
  if(sxIsCall(action))
    return countLocalsInExp(action);
  else if(sxIsSwitch(action))
    return countLocalsInExp(sxSwitchSel(action))+
      countLocalsInActionCases(sxSwitchCases(action));
  else if(sxIsValis(action))
    return countLocalsInExp(sxValisExp(action));
  else if(sxIsConditional(action)){
    long thCount = countLocalsInAct(sxConditionalThen(action));
    long elCount = countLocalsInAct(sxConditionalElse(action));
    return countLocalsInExp(sxConditionalTest(action))+
      (thCount>elCount?thCount:elCount);
  }
  else if(sxIsBlock(action)){
    long count = 0;
    lxPo acts = sxBlockContent(action);
    int size = sxLength(acts);
    long max = 0;
    for(int ix=0;ix<size;ix++){
      sxPo act = sxEl(acts,ix);
      if(sxIsBlock(act)){	      /* Blocks in blocks introduce new scope */
	int lCount = countLocalsInAct(act);
	if(lCount>max)			/* look for the high water mark */
	  max = lCount;
      }
      else
	count+=countLocalsInAct(act);
    }
    return count+max;
  }
  else if(sxIsAssignment(action))
    return 0;
  else if(sxIsLabeled(action))
    return countLocalsInAct(sxLabeledBody(action));
  else if(sxIsLoopAction(action))
    return countLocalsInAct(sxLoopBody(action));
  else if(sxIsWhile(action))
    return countLocalsInAct(sxWhileBody(action));
  else if(sxIsIsDeclaration(action))
    return countLocalsInPtn(sxDeclLval(action));
  else if(sxIsVarDeclaration(action))
    return countLocalsInPtn(sxDeclLval(action));
  else if(sxIsCatch(action)){
    long mx = countLocalsInAct(sxCatchBody(action));
    long catches = countLocalsInActionCases(sxCatchClauses(action));
    if(catches>mx)
      mx = catches;
    return mx;
  }
  else
    return 0;
}

long countLocalsInBlock(lxPo acts)
{
  long count = 0;
  int size = sxLength(acts);
  for(int ix=0;ix<size;ix++)
    count+=countLocalsInAct(sxEl(acts,ix));
  return count;
}

long countLocalsInActionCase(sxPo cse)
{
  if(sxIsDefaultRule(cse))
    return countLocalsInAct(sxDefltBody(cse));
  else
    return countLocalsInPtn(sxCasePtn(cse))+
      countLocalsInAct(sxCaseBody(cse));
}

static long countLocalsInActionCases(lxPo cases)
{
  long arity = sxLength(cases);

  long max = 0;

  for(int ix=0;ix<arity;ix++){
    sxPo cse = sxEl(cases,ix);
    long cCount;

    if(sxIsCaseRule(cse))
      cCount = countLocalsInActionCase(cse);
    else if(sxIsDefaultRule(cse))
      cCount = countLocalsInAct(sxDefltBody(cse));

    if(cCount>max)
      max = cCount;
  }
  return max;
}
