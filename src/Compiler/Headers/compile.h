#ifndef _COMPILE_H_
#define _COMPILE_H_

#include <stdarg.h>

#include "meta.h"
#include "dict.h"
#include "method.h"
#include "assem.h"

extern void initCompiler();
extern void initConstructors();

typedef retCode (*contFun)(locationPo loc,varInfoPo src,void *cl,assemPo code);

typedef struct _exit_stack_ *exitPo;

typedef struct _exit_stack_ {
  uniChar *name;
  contFun cont;
  void *cl;
  exitPo outer;
} ExitLabel;

extern exitPo exitLabel(exitPo exit,uniChar *name);

typedef retCode (*compileFun)(sxPo exp,sxPo *expected,
			      uniChar *path,
			      dictPo dict,dictPo outer,
			      exitPo exit, mtdPo mtd, 
			      contFun cont,void *cl);

extern retCode compileAndGo(uniChar *path,int argc, char **args);

extern retCode compileTheta(lxPo defs,uniChar *path,
			    dictPo dict,dictPo outer,
			    compileFun bound,sxPo *expected,sxPo cl,
			    exitPo exit,mtdPo mtd,
			    contFun cont,void *ccl);

extern retCode declareArgs(lxPo args,dictPo fDict,dictPo outer,mtdPo mtd);


extern retCode compileExp(sxPo exp,sxPo *expectedType,
			  uniChar *path,
			  dictPo dict,dictPo outer,
			  exitPo exit,mtdPo mtd,
			  contFun cont,void *cl);

extern retCode compileArgs(lxPo args,lxPo argTypes,
			   int depth,uniChar *path,
			   dictPo dict,dictPo outer,exitPo exit,
			   mtdPo mtd);

extern sourceKind expMode(sxPo exp,dictPo dict);

extern retCode compileArithmetic(sxPo exp,sxPo *expected,
				 uniChar *path,
				 dictPo dict,dictPo outer,
				 exitPo exit,
				 mtdPo mtd, 
				 contFun cont,void *cl);

extern retCode compileSwitch(sxPo cse,sxPo *expected,
			     uniChar *path,
			     dictPo dict,dictPo outer,
			     exitPo exit, mtdPo mtd,
			     compileFun comp,
			     contFun cont,void *cl);

extern retCode compileCaseAnalysis(locationPo loc,Register tgt,
				   sxPo *expected,
				   lxPo cases,
				   uniChar *path,
				   dictPo dict,dictPo outer,
				   exitPo exit,mtdPo mtd,
				   compileFun bodyFun,
				   contFun cont,void *cl);

extern retCode compileAction(sxPo act,sxPo *expected,
			     uniChar *path,
			     dictPo dict,dictPo outer,
			     exitPo exit,mtdPo mtd,
			     contFun cont,void *cl);

typedef enum { onSuccess, onFailure } jumpMode;

extern retCode compileCondition(sxPo cond,uniChar *path,
				dictPo dict,dictPo outer,
				jumpMode sense,
				lPo fail,exitPo exit,mtdPo mtd);

extern retCode compileCatch(sxPo act,sxPo *expected,
			    uniChar *path,
			    dictPo dict,dictPo outer,exitPo exit,mtdPo mtd,
			    contFun cont,void *cl);

extern retCode compileThrow(sxPo exp,uniChar *path,
			    dictPo dict, dictPo outer,exitPo exit,mtdPo mtd,
			    contFun cont,void *cl);

extern retCode compileTypeDef(sxPo type,uniChar *path,
			      dictPo dict);

extern retCode compileConstructor(sxPo exp,sxPo *expected,
				  uniChar *path,
				  dictPo dict,dictPo outer,
				  exitPo exit,mtdPo mtd,
				  contFun cont,void *cl);

extern retCode compileRecord(sxPo exp,sxPo *expected,
			     uniChar *path,
			     dictPo dict,dictPo outer,
			     exitPo exit,mtdPo mtd,
			     contFun cont,void *cl);

extern retCode genPtnCode(sxPo con,uniChar *path,dictPo dict,mtdPo mtd);

extern retCode genPtnArgs(lxPo args,uniChar *path,dictPo dict,mtdPo mtd);

extern retCode genCatchBlocks(mtdPo mtd,lPo catch);

typedef struct {
  contFun cont1,cont2;
  void *cl1,*cl2;
} Combo, *comboPo;

extern retCode loadReg(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode loadFpReg(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode assignVar(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode copyToT(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode returnCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode returnCCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode rtnCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode CrtnCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode returnMain(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode memoCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode comboCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode jumpCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode typeConvert(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode nullCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern retCode errorCont(locationPo loc,varInfoPo src,void *cl,assemPo code);

extern logical isJumpCont(contFun cont,void *cl);

extern retCode jumpTarget(assemPo code,lPo lbl);

extern long typeSize(sxPo size);
extern sourceKind typeRep(sxPo type);
extern long sourceSize(sourceKind mcType);
extern long argSize(sxPo type);

extern long countLocalsInExp(sxPo exp);
extern long countLocalsInPtn(sxPo ptn);
extern long countLocalsInPtnArgs(lxPo args);
extern long countLocalsInAct(sxPo act);
extern long countLocalsInBlock(lxPo acts);
extern long countConstructors(lxPo defs);
extern long countLocalsInDefs(lxPo defs);

extern logical isLeafExp(sxPo exp,dictPo dict);
extern logical isLeafAct(sxPo exp,dictPo dict);
extern logical isLeafBlock(lxPo acts,dictPo dict);

extern double getNanoTime();

extern sxPo voidSpec;

#endif
