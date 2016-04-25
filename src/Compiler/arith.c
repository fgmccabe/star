/*
 * Handle compilation of arithmetic expressions
 */

#include "compiler.h"
#include "dict.h"
#include "compile.h"
#include "escapes.h"
#include "meta.h"
#include "arith.h"
#include "codegen.h"
#include "utils.h"

static retCode compileFpArithmetic(sxPo exp,uniChar *path,
				   dictPo dict,dictPo outer,
				   exitPo exit,
				   mtdCxtPo mtd,
				   int stkDepth,contFun cont,void *cl);
static retCode compileArith(sxPo exp,sxPo *expected,
			    sourceKind kind,
			    uniChar *path,
			    dictPo dict,dictPo outer,
			    exitPo exit, mtdCxtPo mtd,
			    int stkDepth,contFun cont,void *cl);

static Register stkRegs[] = {R0, R1, R2, R3};
static FpRegister stkFpRegs[] = {FPR0, FPR1, FPR2, FPR3};

retCode compileArithmetic(sxPo exp,sxPo *expected,
			  uniChar *path,
			  dictPo dict,dictPo outer,
			  exitPo exit,
			  mtdCxtPo mtd,contFun cont,void *cl)
{
  sourceKind kind = expMode(exp,dict);

  switch(kind){
  case rawChar:
  case rawInt:
  case rawLong:
    return compileArith(exp,expected,kind,path,dict,outer,
			exit,mtd,0,cont,cl);
  case rawFloat:
    return compileFpArithmetic(exp,path,dict,outer,
			       exit,mtd,0,cont,cl);
  default:
    reportError(sxLoc(exp),"invalid arithmetic expression: %A",exp);
    return Error;
  }
}

retCode compileArith(sxPo exp,sxPo *expected,sourceKind kind,
		     uniChar *path,
		     dictPo dict,dictPo outer,
		     exitPo exit, mtdCxtPo mtd,
		     int stkDepth,contFun cont,void *cl)
{
  assemPo code = methodCode(mtd);
  sxPo lhs = sxLhs(exp);
  sxPo rhs = sxRhs(exp);
  locationPo loc = sxLoc(exp);

  VarInfoRec lSrc = {.loc=sxLoc(lhs), .where=registr, .kind=kind,
		     .l.reg=stkRegs[stkDepth]};
  VarInfoRec rSrc = {.loc=sxLoc(rhs), .where=registr, .kind=kind, 
		     .l.reg=stkRegs[stkDepth+1]};

  exitPo overflow = exitLabel(exit,ArithOverFlow);
  lPo fail = overflow!=Null?(lPo)overflow->cl:Null;

  if(sxIsArithExp(lhs))
    tryRet(compileArith(lhs,expected,kind,
			path,dict,outer,
			exit,mtd,stkDepth,loadReg,&lSrc.l.reg));
  else
    tryRet(compileExp(lhs,expected,
		      path,dict,outer,exit,mtd,loadReg,&lSrc.l.reg));

  if(sxIsArithExp(rhs)){
    if(stkDepth<NumberOf(stkRegs))
      tryRet(compileArith(rhs,expected,kind,path,dict,outer,
			  exit,mtd,stkDepth,loadReg,&rSrc.l.reg));
    else{
      reportError(loc,"arithmetic expression %A too deep",exp);
      return Error;
    }
  }
  else if(sxIsInt(rhs) && sxInt(rhs)==1){ // Allow for literals (just 1 for now)
    rSrc.where = literal;
    rSrc.kind = rawInt;
    rSrc.l.ix = 1;
  }
  else
    tryRet(compileExp(rhs,expected,
		      path,dict,outer,exit,mtd,loadReg,&rSrc.l.reg));

  uniChar* op = sxArithOp(exp);
  if(op==AddOp){
    if(rSrc.where==literal){
      switch(kind){
      case rawChar:
      	AIncC(code,lSrc.l.reg);
      	break;
      case rawInt:
      	AIncI(code,lSrc.l.reg);
      	break;
      case rawLong:
      	AIncL(code,lSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
    }
    else{
      switch(kind){
      case rawChar:
      	AAddC(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawInt:
      	AAddI(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawLong:
      	AAddL(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
    }
    if(fail!=Null)
      ABov(code,fail);
  }
  else if(op==SubtractOp){
    if(rSrc.where==literal){
      switch(kind){
      case rawChar:
      	ADecC(code,lSrc.l.reg);
      	break;
      case rawInt:
      	ADecI(code,lSrc.l.reg);
      	break;
      case rawLong:
      	ADecL(code,lSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
    }
    else{
      switch(kind){
      case rawChar:
      	ASubC(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawInt:
      	ASubI(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawLong:
      	ASubL(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
    }
    if(fail!=Null)
      ABov(code,fail);
  }
  else if(op==TimesOp){
    if(rSrc.where==literal)
      ;
    else{
      switch(kind){
      case rawChar:
      	AMulC(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawInt:
      	AMulI(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawLong:
      	AMulL(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
    }
    if(fail!=Null)
      ABov(code,fail);
  }
  else if(op==DivideOp){
    if(rSrc.where==literal)
      ;
    else{
      exitPo zero = exitLabel(exit,ArithZeroDivide);

      switch(kind){
      case rawChar:
      	if(zero!=Null)
      	  ABzC(code,rSrc.l.reg,(lPo)zero->cl);
      	ADivC(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawInt:
      	if(zero!=Null)
      	  ABzI(code,rSrc.l.reg,(lPo)zero->cl);
      	ADivI(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawLong:
      	if(zero!=Null)
      	  ABzL(code,rSrc.l.reg,(lPo)zero->cl);
      	ADivL(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
      if(fail!=Null)
      	ABov(code,fail);
    }
  }
  else if(op==RemainderOp){
    if(rSrc.where==literal)
      ;
    else{
      exitPo zero = exitLabel(exit,ArithZeroDivide);
      switch(kind){
      case rawChar:
      	if(zero!=Null)
      	  ABzC(code,rSrc.l.reg,(lPo)zero->cl);
      	ARemC(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawInt:
      	if(zero!=Null)
      	  ABzI(code,rSrc.l.reg,(lPo)zero->cl);
      	ARemI(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawLong:
      	if(zero!=Null)
      	  ABzL(code,rSrc.l.reg,(lPo)zero->cl);
      	ARemL(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
      if(fail!=Null)
      	ABov(code,fail);
    }
  }
  else if(op==LshiftOp){
    if(rSrc.where==literal)
      reportError(loc,"literal shift not yet implemented: %A",exp);
    else{
      switch(kind){
      case rawChar:
      	ALeftC(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawInt:
      	ALeftI(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawLong:
      	ALeftL(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
    }
  }
  else if(op==RshiftOp){
    if(rSrc.where==literal)
      reportError(loc,"literal shift not yet implemented: %A",exp);
    else{
      switch(kind){
      case rawChar:
      	ARightC(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawInt:
      	ARightI(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      case rawLong:
      	ARightL(code,lSrc.l.reg,rSrc.l.reg);
      	break;
      default:
      	reportError(loc,"invalid arithmetic expression: %A",exp);
      	return Error;
      }
    }
  }
  else{
    reportError(loc,"invalid arithmetic expression: %A",exp);
    return Error;
  }

  return cont(loc,&lSrc,cl,code);
}

retCode compileFpArithmetic(sxPo exp,uniChar *path,
			    dictPo dict,dictPo outer,
			    exitPo exit,
			    mtdCxtPo mtd, int stkDepth,contFun cont,void *cl)
{
  assemPo code = methodCode(mtd);
  sxPo lhs = sxLhs(exp);
  sxPo rhs = sxRhs(exp);
  locationPo loc = sxLoc(exp);

  VarInfoRec lSrc = {.loc=sxLoc(lhs), .where=registr, .kind=rawFloat,
		     .l.fpReg=stkFpRegs[stkDepth]};
  VarInfoRec rSrc = {.loc=sxLoc(rhs), .where=registr, .kind=rawFloat, 
		     .l.fpReg=stkFpRegs[stkDepth+1]};

  exitPo overflow = exitLabel(exit,ArithOverFlow);
  lPo fail = overflow!=Null?(lPo)overflow->cl:Null;

  if(sxIsArithExp(lhs))
    tryRet(compileFpArithmetic(lhs,path,dict,outer,
			       exit,mtd,stkDepth,loadFpReg,
			       &lSrc.l.fpReg));
  else
    tryRet(compileExp(lhs,&floatType,
		      path,dict,outer,exit,mtd,loadFpReg,&lSrc.l.fpReg));

  if(sxIsArithExp(rhs)){
    if(stkDepth<NumberOf(stkFpRegs))
      tryRet(compileFpArithmetic(rhs,path,dict,outer,exit,mtd,stkDepth,
				 loadFpReg,&rSrc.l.fpReg));
    else{
      reportError(loc,"arithmetic expression %A too deep",exp);
      return Error;
    }
  }
  else
    tryRet(compileExp(rhs,&floatType,
		      path,dict,outer,exit,mtd,loadFpReg,&rSrc.l.fpReg));

  uniChar *op = sxArithOp(exp);
  if(op==AddOp)
    AAddD(code,lSrc.l.fpReg,rSrc.l.fpReg);
  else if(op==SubtractOp)
    ASubD(code,lSrc.l.fpReg,rSrc.l.fpReg);
  else if(op==TimesOp)
    AMulD(code,lSrc.l.fpReg,rSrc.l.fpReg);
  else if(op==DivideOp){
    exitPo zero = exitLabel(exit,ArithZeroDivide);

    if(zero!=Null)
      ABzD(code,rSrc.l.fpReg,(lPo)zero->cl);

    ADivD(code,lSrc.l.fpReg,rSrc.l.fpReg);
  }
  else if(op==RemainderOp){
    reportError(loc,"floating point remainder not supported");
    return Error;
  }
  else{
    reportError(loc,"invalid arithmetic expression ");
    return Error;
  }
  if(fail!=Null)
    ABov(code,fail);

  return cont(loc,&lSrc,cl,code);
}

sourceKind expMode(sxPo exp,dictPo dict)
{
  if(sxIsArithExp(exp)){
    sourceKind lMode = expMode(sxLhs(exp),dict);
    sourceKind rMode = expMode(sxRhs(exp),dict);
    if(lMode!=rMode)
      reportError(sxLoc(exp),"cannot mix arithmetic modes");
    return lMode;
  }
  else if(sxIsIden(exp)){
    uniChar *vrName = sxIden(exp);
    varInfoPo var = varReference(vrName,dict);

    if(var!=Null)
      return var->kind;
    else{
      reportError(sxLoc(exp),"%A not defined",exp);
      return general;
    }
  }
  else if(sxIsChar(exp))
    return rawChar;
  else if(sxIsInt(exp))
    return rawInt;
  else if(sxIsLong(exp))
    return rawLong;
  else if(sxIsFloat(exp))
    return rawFloat;
  else if(sxIsCast(exp)){
    sxPo type = sxCastType(exp);
    if(isRawFloatType(type))
      return rawFloat;
    else if(isRawCharType(type))
      return rawChar;
    else if(isRawIntType(type))
      return rawInt;
    else if(isRawLongType(type))
      return rawLong;
    else
      return general;
  }
  else
    return general;
}

uniChar *AddOp, *SubtractOp, *TimesOp, *DivideOp, *RemainderOp,
  *LshiftOp, *RshiftOp, *BitAndOp, *BitOrOp, *BitXorOp, *BitNegOp;

retCode initArith()
{
  AddOp = mkInterned("+");
  SubtractOp = mkInterned("-");
  TimesOp = mkInterned("*");
  DivideOp = mkInterned("/");
  RemainderOp = mkInterned("%");
  LshiftOp = mkInterned("<<");
  RshiftOp = mkInterned(">>");
  BitAndOp = mkInterned(".&.");
  BitOrOp = mkInterned(".|.");
  BitXorOp = mkInterned(".^.");
  BitNegOp = mkInterned(".~.");
  return Ok;
}
