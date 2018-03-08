/*
 * Handle the built-in escapes.
 *
 * There are two kinds of escapes -- those implemented in C and those
 * implemented by generating code directly for them. They are both handled the
 * same way from the code generator's pov.
 */
#include "compiler.h"
#include "meta.h"
#include "dict.h"

#include "compile.h"
#include "codegen.h"
#include "funlibs.h"
#include "arith.h"

#include "hash.h"
#include "pool.h"


static hashPo escapes;


#define DECLARE_LIT_ARITY 3

void initEscapes()
{
  escapes = NewHash(255,(hashFun)uniHash, (compFun)uniCmp,NULL);

  initArith();

  lxPo args = mCons(stringType,nil);

  defineLibFun("__#break",sxArrowType(Null,args,voidType),(cFunPo)stopHere);
  defineLibFun("__#nanos",sxArrowType(Null,nil,longType),(cFunPo)nanos);
  defineLibFun("__#exit",sxArrowType(Null,args,voidType),(cFunPo)exit);
  defineLibFun("__#syserr",sxArrowType(Null,args,voidType),(cFunPo)syserr);
  defineLibFun("__#memerr",sxArrowType(Null,nil,voidType),(cFunPo)memerr);
  defineLibFun("__#sleep",sxArrowType(Null,mCons(longType,nil),voidType),
	       (cFunPo)sleep);

  initIoFuns();
}

retCode compileEscape(locationPo loc,char *name,sxPo *expected,
		      char *path,
		      dictPo dict,dictPo outer,
		      exitPo exit,
		      mtdCxtPo mtd,lxPo args,
		      contFun cont,void *cl)
{
  escapeFun escape = Search(name,escapes);

  if(escape!=NULL)
    return escape(loc,expected,path,dict,outer,exit,mtd,args,cont,cl);
  else{
    reportError(loc,"%U not defined",name);
    return Error;
  }
}

/*
 * C functions are declared with a structure that defines the type of the
 * function.
 */

static int regArgCount(sxPo type);
static int fpArgCount(sxPo type);

static Register cArgRegs[] = { R1, R2, R3, R4 };
static FpRegister cFpArgRegs[] = {FPR0, FPR1, FPR2, FPR3};

typedef struct {
  int ix,fx;
} ArgRegSpec;

static retCode loadArgReg(locationPo loc,varInfoPo src,void *cl,assemPo code);

retCode compileCCall(sxPo call,sxPo *expected,
		     char *path,
		     dictPo dict,dictPo outer,
		     exitPo exit,mtdCxtPo mtd,
		     contFun cont,void *cl)
{
  assemPo code = methodCode(mtd);
  locationPo loc = sxLoc(call);
  char * name = sxCallOp(call);
  lxPo args = sxCallArgs(call);
  varInfoPo lib = findLibFun(name);

  if(lib==Null){
    reportError(loc,"library function %U not known",name);
    return Error;
  }

  AStartCCall(code,localDepth(dict),regArgCount(lib->type),fpArgCount(lib->type));

  long arity = sxLength(args);

  if(arity>NumberOf(cArgRegs)){
    reportError(loc,"too many arguments to C call: %A",call);
    return Error;
  }
  else{
    ArgRegSpec argRegs = {.ix=0,.fx=0};
    retCode ret = Ok;

    for(int ix=0;ret==Ok && ix<arity;ix++){
      sxPo argType = arrowtypeArg(lib->type,ix);
      ret = compileExp(sxEl(args,ix),&argType,
		       path,dict,outer,exit,mtd,loadArgReg,&argRegs);
    }

    ACallClbl(code,lib->l.lit);
    gcCallSite(mtd,dict);		/* In case gc was invoked */
    ALd(code,ENV,FP,ENV_ARG_OFFSET);	/* reload the environment */

    sxPo resType = arrowResType(lib->type);

    if(isRawCharType(resType)){
      VarInfoRec res = { .loc=loc, .where=registr,
			 .kind=rawChar,.type=resType, .l.reg = R0 };
      return cont(loc,&res,cl,code);
    }
    else if(isRawIntType(resType)){
      VarInfoRec res = { .loc=loc,
			 .where=registr,
			 .kind=rawInt,.type=resType,
			 .l.reg = R0 };
      return cont(loc,&res,cl,code);
    }
    else if(isRawLongType(resType)){
      VarInfoRec res = { .loc=loc,
			 .where=registr,
			 .kind=rawLong, .type=resType,
			 .l.reg = R0 };
      return cont(loc,&res,cl,code);
    }
    else if(isRawFloatType(resType)){
      VarInfoRec res = { .loc=loc,
			 .where=registr,
			 .kind=rawFloat, .type=resType,
			 .l.fpReg = FPR0 };
      return cont(loc,&res,cl,code);
    }
    else{
      VarInfoRec res = { .loc=loc,
			 .where=registr,
			 .kind=general,.type=resType,
			 .l.reg = R0 };
      return cont(loc,&res,cl,code);
    }
  }
}

retCode loadArgReg(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  ArgRegSpec *spec = (ArgRegSpec*)cl;

  switch(src->kind){
  case rawFloat:{
    FpRegister reg = cFpArgRegs[spec->fx++];
    return loadFpReg(loc,src,&reg,code);
  }
  default:{
    Register reg = cArgRegs[spec->ix++];
    return loadReg(loc,src,&reg,code);
  }
  }
}

int regArgCount(sxPo type)
{
  int count = 0;
  int ar = sxLength(arrowArgTypes(type));

  for(int ix=0;ix<ar;ix++){
    sxPo argType = arrowtypeArg(type,ix);
    if(!isRawFloatType(argType))
      count++;
  }
  return count;
}

static int fpArgCount(sxPo type)
{
  int ar = sxLength(arrowArgTypes(type));
  int count = 0;

  for(int ix=0;ix<ar;ix++){
    sxPo argType = arrowtypeArg(type,ix);
    if(isRawFloatType(argType))
      count++;
  }
  return count;
}

retCode compileLibVar(locationPo loc,char *name,
		      sxPo *expected,
		      char *path,
		      dictPo dict,dictPo outer,
		      exitPo exit, mtdCxtPo mtd,
		      contFun cont,void *cl)
{
  varInfoPo var = findLibVar(name);
  assemPo code = methodCode(mtd);

  if(var==Null){
    reportError(loc,"library var: %U not defined",name);
    return Error;
  } else if(checkType(var->type,expected,loc)!=Ok){
    reportError(loc,"library var: %U:%T not consistent with expected type: %T",
		name,var->type,*expected);
    return Error;
  }

  return cont(loc,var,cl,code);
}

logical isEscape(char *name)
{
  return Search(name,escapes)!=Null;
}

void defineEscape(char *name, escapeFun escape)
{
  char nameU[1024];

  _uni((unsigned char*)name,nameU,NumberOf(nameU));
  Install(uniIntern(nameU),escape,escapes);
}

void genDeclare(assemPo code,varInfoPo info,dictPo dict)
{
  AStartCCall(code,localDepth(dict),DECLARE_LIT_ARITY,0);
  int regIx = 0;
  VarInfoRec src = {.loc=info->loc,.where=literal,.kind=general};

  Register reg = cArgRegs[regIx++];
  src.l.str = info->l.str;
  loadReg(info->loc,info,&reg,code);	/* info */

  reg = cArgRegs[regIx++];
  loadReg(info->loc,info,&reg,code);	/* pick up the value */

  reg = cArgRegs[regIx++];
  src.l.bx = dict;
  loadReg(info->loc,&src,&reg,code);	/* dict */

  ACallLib(code,(libFun)declareInfo);
}

static retCode dEsc(char *n,escapeFun esc,void *c)
{
  return outMsg(logFile,"%U->%x",n,esc);
}

void dumpEscapes()
{
  ProcessTable((procFun)dEsc,escapes,Null);
  flushOut();
}
