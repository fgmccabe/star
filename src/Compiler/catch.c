/*
 * Handle compilation of catch and throws
 */

#include "compiler.h"
#include "dict.h"
#include "utils.h"
#include "meta.h"
#include "compile.h"
#include "codegen.h"

retCode compileCatch(sxPo act,sxPo *expected,
		     char *path,
		     dictPo dict,dictPo outer,
		     exitPo exit,
		     mtdCxtPo mtd,
		     contFun cont,void *cl)
{
  assemPo code = methodCode(mtd);
  sxPo body = sxCatchBody(act);
  lxPo clauses = sxCatchClauses(act);
  locationPo loc = sxLoc(act);

  exitPo catchExit = exitLabel(exit,CatchName);

  lPo start = currLbl(code,genSym(".T"));
  lPo recover = newLbl(code,genSym(".R"));
  ExitLabel catch = {.name=CatchName,.cont=jumpCont,.cl=recover,
		     .outer = catchExit};

  tryRet(compileAction(body,expected,path,dict,outer,&catch,mtd,cont,cl));

  lPo end = currLbl(code,genSym(".E"));
  
  char *currSeg = currSegment(code);
  setSegment(code,genSym(".CC"));
  
  defineLbl(code,recover);
  tryRet(compileCaseAnalysis(loc,R0,expected,clauses,
			     path,dict,outer,
			     exit,mtd,compileAction,cont,cl));
  addCatchBlock(mtd,start,end,recover);
  setSegment(code,currSeg);
  return Ok;
}

retCode genCatchBlocks(mtdCxtPo mtd,lPo catch)
{
  assemPo code = methodCode(mtd);
  AAlignTo(code,POINTER_SIZE);
  defineLbl(code,catch);
  tryPo catches = methodCatchBlocks(mtd);
  while(catches!=Null){
    AConstP(code,tryBlockStart(catches));
    AConstP(code,tryBlockEnd(catches));
    AConstP(code,tryBlockRecover(catches));
    catches = tryBlockNext(catches);
  }
  AConstI(code,0);			/* Mark the end of the table */
  return Ok;
}
