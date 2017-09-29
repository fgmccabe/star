/*
 * Handle compilation of case expressions and actions.
 */

#include "compiler.h"
#include "dict.h"
#include "utils.h"
#include "compile.h"
#include "codegen.h"

#include <stdlib.h>

int findConstructorMax(lxPo cases, dictPo dict)
{
  int count = sxLength(cases);
  int max = -1;
  for(int ix=0;ix<count;ix++){
    sxPo csePtn = sxCasePtn(sxEl(cases,ix));
    if(sxIsConstructor(csePtn)){
      char *con = sxConstructorOp(csePtn);
      conDefPo def = findConstructor(con,dict);
      if(def==Null){
	reportError(sxLoc(csePtn),"constructor %A not declared",csePtn);
	return 0;
      }
      if(def->maxIx>max)
	max = def->maxIx;
      continue;
    }
    else if(sxIsIden(csePtn)){
      char *con = sxIden(csePtn);
      conDefPo def = findConstructor(con,dict);
      if(def==Null){
	reportError(sxLoc(csePtn),"symbol %A not declared",csePtn);
	return 0;
      }
      else if(sxLength(def->args)!=0){
	reportError(sxLoc(csePtn),"symbol %A requires %d args",
		    csePtn,sxLength(def->args));
	return 0;
      }
      else if(def->maxIx>max)
	max = def->maxIx;
      continue;
    }
    else
      reportError(sxLoc(csePtn),"invalid pattern in case: %A",csePtn);
  }
  return max;
}

retCode compileSwitch(sxPo caseExp,sxPo *expected,
		      char *path,
		      dictPo dict,dictPo outer,
		      exitPo exit, mtdCxtPo mtd,
		      compileFun bodyFun,contFun cont,void *cl)
{
  Register tgt = R0;
  sxPo sel = sxSwitchSel(caseExp);
  lxPo cases = sxSwitchCases(caseExp);
  sxPo selType = Null;

  retCode ret = compileExp(sel,&selType,path,dict,outer,
			   exit,mtd,loadReg,&tgt);
  if(ret==Ok)
    return compileCaseAnalysis(sxLoc(sel),tgt,expected,cases,
			       path,dict,outer,exit,mtd,bodyFun,cont,cl);
  else
    return ret;
}

retCode compileCaseAnalysis(locationPo loc,Register tgt,sxPo *expected,
			    lxPo cases,char *path,
			    dictPo dict,dictPo outer,
			    exitPo exit, mtdCxtPo mtd,
			    compileFun bodyFun,contFun cont,void *cl)
{
  int maxIx = findConstructorMax(cases,dict)/POINTER_SIZE;
  assemPo code = methodCode(mtd);
  lPo labels[maxIx];
  sxPo defltCase = Null;
  lPo deflt = newLbl(code,genSym(".D"));
  lPo caseLabels = newLbl(code,genSym(".C"));
  retCode res = Ok;

  char *seg = currSegment(code);

  ACase(code,tgt,maxIx,caseLabels);	/* Do the case jump */

  for(int ix=0;ix<maxIx;ix++)
    labels[ix] = deflt;			/* Exit strategy */

  for(int ix=0;res==Ok && ix<maxIx;ix++){
    sxPo cse = sxEl(cases,ix);

    if(sxIsCaseRule(cse)){
      sxPo ptn = sxCasePtn(cse);
      sxPo body = sxCaseBody(cse);
      dictStatePo caseState = dictState(dict);
      setSegment(code,genSym(".C"));	/* each case in a different segment */

      if(sxIsConstructor(ptn)){
	char *con = sxConstructorOp(ptn);
	conDefPo def = findConstructor(con,dict);
	
	lPo cseLbl = currLbl(code,genSym(".L"));
	labels[def->conIx/POINTER_SIZE] = cseLbl;
	res = genPtnCode(ptn,path,dict,mtd);
	if(res==Ok)
	  res = bodyFun(body,expected,path,dict,outer,exit,mtd,cont,cl);
      } else if(sxIsIden(ptn)){
	char *con = sxIden(ptn);
	conDefPo def = findConstructor(con,dict);
	if(def!=Null){
	  if(def->conSize!=0){
	    reportError(sxLoc(ptn),"%U is not an enumerated symbol",con);
	    res = Error;
	  }
	  else{
	    lPo cseLbl = currLbl(code,genSym(".L"));
	    labels[def->conIx/POINTER_SIZE] = cseLbl;
	    res = bodyFun(body,expected,path,dict,outer,exit,mtd,cont,cl);
	  }
	}
	else{
	  reportError(sxLoc(ptn),"%U is not an enumerated symbol",con);
	  res = Error;
	}
      }
      else{
	reportError(sxLoc(ptn),"invalid pattern: %A in case",ptn);
	return Error;
      }
      resetDict(dict,caseState);
    }
    else if(sxIsDefaultRule(cse))
      defltCase = cse;
  }

  setSegment(code,seg);			/* Now insert the jump table */

  AAlignTo(code,POINTER_SIZE);
  defineLbl(code,caseLabels);		/* plant the jump table */
  logical usedDeflt = False;
  for(int ix=0;res==Ok&&ix<maxIx;ix++){
    AConstP(code,labels[ix]);
    
    if(labels[ix]==deflt)
      usedDeflt = True;
  }

  if(res==Ok){
    if(usedDeflt){
      if(defltCase!=Null){
	defineLbl(code,deflt);		/* This is the default exit */
	dictStatePo caseState = dictState(dict);

	res = genPtnCode(sxCasePtn(defltCase),path,dict,mtd);
	if(res==Ok)
	  res = bodyFun(sxCaseBody(defltCase),expected,path,
			dict,outer,exit,mtd,cont,cl);
	resetDict(dict,caseState);
      }
      else{
	reportError(loc,"missing cases and no default provided");
	return Error;
      }
    } else if(defltCase!=Null)
      reportWarning(sxLoc(defltCase),"default case is dead code");
  }
  return res;
}

retCode compileScalarCase(locationPo loc,varInfoPo src,sxPo *expected,
			  lxPo cases,char *path,
			  dictPo dict,dictPo outer,
			  exitPo exit, mtdCxtPo mtd,
			  compileFun bodyFun,contFun cont,void *cl)
{
  int count = sxLength(cases);
  assemPo code = methodCode(mtd);
  lPo labels[count];
  sxPo defltCase = Null;
  lPo deflt = newLbl(code,genSym(".D"));
  lPo caseLabels = newLbl(code,genSym(".C"));
  lPo endLabels = newLbl(code,genSym(".CE"));

  retCode res = Ok;

  Register tgt = R0;

  loadReg(loc,src,&tgt,code);

  // The search algorithm is:
  // while(limit>base){
  //   probe = (limit+base)/2;
  //   if(probe->key==test)
  //     jmp probe->lbl;
  //   else if(probe->key<test)
  //     limit = probe;
  //   else
  //     base = probe;
  // }
  // <deflt>

  Register base = R1;
  Register probe = R2;
  Register limit = R3;
  Register test = R4;

  AMoveLbl(code,base,caseLabels);
  AMoveLbl(code,limit,endLabels);

  lPo caseLoop = currLbl(code,genSym(".loop"));
  ABge(code,base,limit,deflt);
  AMove(code,probe,base);
  AAddL(code,probe,limit);
  ARsL(code,probe,1);			/* probe = (base+limit)/2 */
  ALdI(code,test,probe,0);
  lPo skipEq = newLbl(code,genSym(".s"));
  ABne(code,test,tgt,skipEq);
  ALd(code,test,probe,LONG_SIZE);	/* test = probe->lbl */
  AJmpR(code,test);
  defineLbl(code,skipEq);
  lPo skipLt = newLbl(code,genSym(".l"));
  ABgt(code,test,tgt,skipLt);
  AMove(code,limit,probe);		/* limit := probe */
  AJmp(code,caseLoop);
  defineLbl(code,skipLt);
  AMove(code,base,probe);		/* base := probe */
  AJmp(code,caseLoop);

  AAlignTo(code,POINTER_SIZE);
  defineLbl(code,caseLabels);		/* plant the jump table */

  for(int ix=0;res==Ok && ix<count;ix++){
    sxPo cse = sxEl(cases,ix);

    if(sxIsCaseRule(cse)){
      sxPo ptn = sxCasePtn(cse);

      lPo caseLbl = labels[ix] = newLbl(code,genSym("Cs"));

      if(sxIsChar(ptn)){
	AConstL(code,(integer)sxChar(ptn));
	AConstP(code,caseLbl);
      }
      else if(sxIsInt(ptn)){
	AConstL(code,(integer)sxInt(ptn));
	AConstP(code,caseLbl);
      }
      else if(sxIsLong(ptn)){
	AConstL(code,sxLong(ptn));
	AConstP(code,caseLbl);
      }
      else if(sxIsFloat(ptn)){
	AConstD(code,sxFloat(ptn)); 
	AConstP(code,caseLbl);
      }
      else{
	reportError(sxLoc(ptn),"invalid scalar pattern: %A in case",ptn);
	return Error;
      }
    }
    else if(sxIsDefaultRule(cse))
      defltCase = cse;
    else{
      reportError(sxLoc(cse),"invalid case: %A in case",cse);
      return Error;
    }
  }

  defineLbl(code,endLabels);

  // Implement the different cases
  for(int ix=0;res==Ok && ix<count;ix++){
    sxPo cse = sxEl(cases,ix);
    if(sxIsCaseRule(cse)){
      sxPo body = sxCaseBody(cse);
      dictStatePo caseState = dictState(dict);

      lPo caseLbl = labels[ix];

      defineLbl(code,caseLbl); 
      res = bodyFun(body,expected,path,dict,outer,exit,mtd,cont,cl);
      resetDict(dict,caseState);
    }
  }

  if(res==Ok){
    defineLbl(code,deflt);		/* This is the default exit */
    if(defltCase!=Null)
      res = bodyFun(defltCase,expected,path,dict,outer,exit,mtd,cont,cl);
    else
      res = cont(loc,src,cl,code);
  }

  return res;
}


