/*
 * Manage S-Expressions. These are similar to, but not identical to LISP
 * S-expressions.
 *
 */

#include "compiler.h"
#include "sexpP.h"
#include "metaP.h"
#include "dict.h"
#include "type.h"
#include <ooio.h>
#include <assert.h>
#include <stdarg.h>
#include <formioP.h>

// Keywords in the meta-language
uniChar *kwImport;

uniChar *kwType, *kwArrow, *kwDo;

uniChar *kwFunction, *kwProcedure, *kwMemo, *kwPattern, *kwLet, *kwCall, *kwConstructor;

uniChar *kwIs, *kwVar, *kwResource, *kwEqual, *kwMatches;

uniChar *kwAssign, *kwColon, *kwBlock, *kwSync;

uniChar *kwLabel, *kwLeave, *kwGoto, *kwNothing;
uniChar *kwValof, *kwValis, *kwAssert;
uniChar *kwIf, *kwThen, *kwElse;

uniChar *kwContinue, *kwWhile, *kwSwitch, *kwIn, *kwCase, *kwDeflt, *kwOtherwise, *kwLoop;

uniChar *kwTry,*kwCatch,*kwThrow;

uniChar *kwPlus, *kwMinus, *kwTimes, *kwDivide, *kwRemainder;
uniChar *kwShiftLeft, *kwShiftRight;

static retCode displayMeta(ioPo f,void *p,long width,long prec,logical alt);
static retCode displaySexp(ioPo f,void *p,long width,long prec,logical alt);
static retCode displaySeqExp(ioPo f,void *p,long width,long prec,logical alt);

void initMeta()
{
  initSexpressions();
  
  kwImport = mkInterned("import");

  kwType = mkInterned("type");
  kwArrow = mkInterned("=>");
  kwDo = mkInterned("do");

  kwFunction = mkInterned("function");
  kwProcedure = mkInterned("procedure");
  kwMemo = mkInterned("memo");
  kwPattern = mkInterned("pattern");

  kwLet = mkInterned("let");

  kwCall = mkInterned("call");
  kwConstructor = mkInterned("con");

  kwIs = mkInterned("is");
  kwVar = mkInterned("var");
  kwResource = mkInterned("resource");
  kwEqual = mkInterned("=");

  kwMatches = mkInterned("matches");

  kwAssign = mkInterned(":=");
  kwColon = mkInterned(":");

  kwBlock = mkInterned("{}");

  kwValof = mkInterned("valof");
  kwValis = mkInterned("valis");

  kwAssert = mkInterned("assert");

  kwIf = mkInterned("if");
  kwThen = mkInterned("then");
  kwElse = mkInterned("else");

  kwContinue = mkInterned("continue");
  kwWhile = mkInterned("while");
  kwOtherwise = mkInterned("otherwise");
  kwLoop = mkInterned("loop");

  kwSwitch = mkInterned("switch");
  kwIn = mkInterned("in");
  kwCase = mkInterned("->");
  kwDeflt = mkInterned("default");

  kwTry = mkInterned("try");
  kwCatch = mkInterned("catch");
  kwThrow = mkInterned("throw");

  kwLabel = mkInterned("::");
  kwLeave = mkInterned("leave");
  kwGoto = mkInterned("goto");
  kwNothing = mkInterned("nothing");
  kwSync = mkInterned("sync");

  kwPlus = mkInterned("+");
  kwMinus = mkInterned("-");
  kwTimes = mkInterned("*");
  kwDivide = mkInterned("/");
  kwRemainder = mkInterned("%");
  kwShiftLeft = mkInterned("<<");
  kwShiftRight = mkInterned(">>");

  installMsgProc('A',displaySexp);	// extend outMsg to cope with s-expressions
  installMsgProc('S',displaySeqExp);	// extend outMsg to cope with s-expressions
  installMsgProc('M',displayMeta);   // extend outMsg to cope with meta
}

logical sxIsIdentifier(sxPo sx,uniChar *name)
{
  if(sxIsIden(sx))
    return uniCmp(sxIden(sx),name)==0;
  else
    return False;
}

// An import looks like:
// import(<pkg>)
sxPo sxImport(locationPo loc, uniChar *pkg)
{
  return sxUnary(loc,kwImport,mId(loc,pkg));
}

logical sxIsImport(sxPo sx)
{
  return sxIsUnary(sx,kwImport) && sxIsIden(sxArg(sx,0));
}

uniChar *sxImportPkg(sxPo sx)
{
  assert(sxIsImport(sx));

  return sxIden(sxArg(sx,0));
}

// A type definition looks like
// type(<type>, {<constructors>} )

sxPo sxTypeDef(locationPo loc,sxPo type,lxPo constructors)
{
  return sxBinary(loc,kwType,type,sxBlock(loc,constructors));
}

logical sxIsTypeDef(sxPo sx)
{
  return sxIsBinary(sx,kwType);
}

sxPo sxTypeDefType(sxPo sx)
{
  assert(sxIsTypeDef(sx));
  return sxLhs(sx);
}

lxPo sxTypeDefCons(sxPo sx)
{
  assert(sxIsTypeDef(sx));
  return sxBlockContent(sxRhs(sx));
}

static lxPo extractArgTypes(lxPo args)
{
  if(args==nil)
    return nil;
  else{
    sxPo arg = sxHead(args);
    sxPo argType;
    
    if(sxIsCast(arg))
      argType = sxCastType(arg);
    else{
      reportError(sxLoc(arg),"expecting a typed argument, not %A",arg);
      argType = voidType;
    }
    return mCons(argType,extractArgTypes(sxTail(args)));
  }
}

// A function definition looks like:
// <name>:<type> is function(<arg1>,...,<argn>) => <exp>

sxPo sxFunction(locationPo loc,uniChar *name,sxPo type,lxPo args,sxPo sx)
{
  sxPo funType = sxArrowType(loc,extractArgTypes(args),type);
  sxPo lval = sxTypedExp(loc,mId(loc,name),funType);
  sxPo head = sxApply(loc,kwFunction,args);
  sxPo def = sxBinary(loc,kwArrow,head,sx);
  return sxIsDeclaration(loc,lval,def);
}

logical sxIsFunction(sxPo sx)
{
  return sxIsIsDeclaration(sx)
    && sxIsCast(sxDeclLval(sx))
    && sxIsBinary(sxDeclValue(sx),kwArrow)
    && sxIsApply(sxLhs(sxDeclValue(sx)))
    && sxIsIdentifier(sxOp(sxLhs(sxDeclValue(sx))),kwFunction);
}

uniChar *sxFunName(sxPo sx)
{
  assert(sxIsFunction(sx));
  return sxIden(sxCastExp(sxArg(sx,0)));
}

sxPo sxFunType(sxPo sx)
{
  assert(sxIsFunction(sx));

  return sxCastType(sxLhs(sx));
}

sxPo sxFunExp(sxPo sx)
{
  assert(sxIsFunction(sx));

  return sxRhs(sxRhs(sx));
}

lxPo sxFunArgs(sxPo sx)
{
  assert(sxIsFunction(sx)||sxIsProcedure(sx));

  return sxApplyArgs(sxLhs(sxRhs(sx)));
}

// A memo looks like:
// <name>:<type> is memo(exp)
//
sxPo sxMemo(locationPo loc,uniChar *name,sxPo type,sxPo exp)
{
  sxPo funType = sxArrowType(loc,nil,type);
  sxPo lval = sxTypedExp(loc,mId(loc,name),funType);
  sxPo def = sxUnary(loc,kwMemo,exp);
  return sxIsDeclaration(loc,lval,def);
}

logical sxIsMemo(sxPo sx)
{
  return sxIsIsDeclaration(sx)
    && sxIsCast(sxDeclLval(sx))
    && sxIsUnary(sxDeclValue(sx),kwMemo);
}

uniChar *sxMemoName(sxPo sx)
{
  assert(sxIsMemo(sx));
  return sxIden(sxCastExp(sxLhs(sx)));
}

sxPo sxMemoType(sxPo sx)
{
  assert(sxIsMemo(sx));

  return sxCastType(sxLhs(sx));
}

sxPo sxMemoExp(sxPo sx)
{
  assert(sxIsMemo(sx));

  return sxUnaryArg(sxRhs(sx));
}

// A procedure looks like:
// <name>:<type> is procedure(<arg1>,...,<argn>) do <action>
sxPo sxProcedure(locationPo loc,uniChar *name,lxPo args,sxPo body)
{
  sxPo prcType = sxArrowType(loc,extractArgTypes(args),voidType);
  sxPo lval = sxTypedExp(loc,mId(loc,name),prcType);
  sxPo head = sxApply(loc,kwProcedure,args);
  sxPo def = sxBinary(loc,kwDo,head,body);
  return sxIsDeclaration(loc,lval,def);
} 

logical sxIsProcedure(sxPo sx)
{
  return sxIsIsDeclaration(sx)
    && sxIsCast(sxDeclLval(sx))
    && sxIsBinary(sxDeclValue(sx),kwDo)
    && sxIsApply(sxLhs(sxDeclValue(sx)))
    && sxIsIdentifier(sxOp(sxLhs(sxDeclValue(sx))),kwProcedure);
}

uniChar *sxProcName(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxIden(sxCastExp(sxLhs(sx)));
}

lxPo sxProcArgs(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxApplyArgs(sxLhs(sxRhs(sx)));
}

sxPo sxProcType(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxCastType(sxLhs(sx));
}

sxPo sxProcBody(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxRhs(sxRhs(sx));
}

// A pattern looks like:
// pattern <name>(<id>:<type>,...,<id>:<type>) matches <pattern>:<type> [if <condition>]
sxPo sxPattern(locationPo loc,uniChar *name,lxPo args,sxPo type,
		      sxPo ptn,sxPo cond)
{
  sxPo ptnType = sxPttrnType(loc,extractArgTypes(args),type);
  sxPo lval = sxTypedExp(loc,mId(loc,name),ptnType);
  sxPo head = sxApply(loc,kwPattern,args);
  sxPo body = (cond==Null?ptn:sxBinary(loc,kwIf,ptn,cond));
  sxPo def = sxBinary(loc,kwMatches,head,body);
  return sxIsDeclaration(loc,lval,def);
} 

logical sxIsPattern(sxPo sx)
{
  return sxIsIsDeclaration(sx)
    && sxIsCast(sxDeclLval(sx))
    && sxIsBinary(sxDeclValue(sx),kwDo)
    && sxIsApply(sxLhs(sxDeclValue(sx)))
    && sxIsIdentifier(sxOp(sxLhs(sxDeclValue(sx))),kwProcedure);
}

uniChar *sxPtnName(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxIden(sxCastExp(sxLhs(sx)));
}

lxPo sxPtnArgs(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxApplyArgs(sxLhs(sxRhs(sx)));
}

sxPo sxPtnType(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxCastType(sxLhs(sx));
}

sxPo sxPtnBody(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxRhs(sxRhs(sx));
}

sxPo sxPtnCond(sxPo sx)
{
  assert(sxIsProcedure(sx));

  return sxRhs(sxRhs(sx));
}






// An is declaration looks like:
// var:type is exp
sxPo sxIsDeclaration(locationPo loc,sxPo lhs,sxPo rhs)
{
  return sxBinary(loc,kwIs,lhs,rhs);
}

logical sxIsIsDeclaration(sxPo sx)
{
  return sxIsBinary(sx,kwIs);
}

sxPo sxDeclLval(sxPo sx)
{
  assert(sxIsIsDeclaration(sx)|| sxIsVarDeclaration(sx)
	 || sxIsResourceDeclaration(sx));

  return sxLhs(sx);
}

uniChar* sxLvalName(sxPo sx)
{
  return sxIden(sxCastExp(sxDeclLval(sx)));
}

sxPo sxLvalType(sxPo sx)
{
  return sxCastType(sxDeclLval(sx));
}

sxPo sxDeclValue(sxPo sx)
{
  assert(sxIsIsDeclaration(sx)||sxIsVarDeclaration(sx));

  return sxRhs(sx);
}

// A var declaration looks like:
// var name:type := exp
sxPo sxVarDeclaration(locationPo loc,sxPo lhs,sxPo rhs)
{
  return sxBinary(loc,kwVar,lhs,rhs);
}

logical sxIsVarDeclaration(sxPo sx)
{
  return sxIsBinary(sx,kwVar) && sxIsCast(sxLhs(sx));
}

// A resource declaration looks like:
// resource name:type := exp
sxPo sxResourceDeclaration(locationPo loc,sxPo lhs,sxPo rhs)
{
  return sxBinary(loc,kwResource,lhs,rhs);
}

logical sxIsResourceDeclaration(sxPo sx)
{
  return sxIsBinary(sx,kwResource) && sxIsCast(sxLhs(sx));
}

uniChar *sxDeclaredVarName(sxPo sx)
{
  return sxLvalName(sx);
}

sxPo sxAssignment(locationPo loc,sxPo lval,sxPo exp)
{
  return sxBinary(loc,kwAssign,lval,exp);
}

logical sxIsAssignment(sxPo sx)
{
  return sxIsBinary(sx,kwAssign);
}

sxPo sxAsgnLVal(sxPo sx)
{
  assert(sxIsAssignment(sx));
  
  return sxLhs(sx);
}

sxPo sxAsgnRVal(sxPo sx)
{
  assert(sxIsAssignment(sx));
  
  return sxRhs(sx);
}

// A typed expression looks like
// name:type

sxPo sxTypedExp(locationPo loc,sxPo exp,sxPo type)
{
  return sxBinary(loc,kwColon,exp,type);
}

sxPo sxIdent(locationPo loc,uniChar *name,sxPo type)
{
  return sxTypedExp(loc,mId(loc,name),type);
}

logical sxIsCast(sxPo sx)
{
  return sxIsBinary(sx,kwColon);
}

sxPo sxCastExp(sxPo sx)
{
  assert(sxIsCast(sx));

  return sxLhs(sx);
}

sxPo sxCastType(sxPo sx)
{
  assert(sxIsCast(sx));

  return sxRhs(sx);
}

sxPo sxContinue(locationPo loc,uniChar *lbl,lxPo args)
{
  return sxBinary(loc,kwContinue,mId(loc,lbl),sxBlock(loc,args));
}

logical sxIsContinue(sxPo sx)
{
  return sxIsBinary(sx,kwContinue) && sxIsIden(sxLhs(sx)) && 
    sxIsBlock(sxRhs(sx));
}

uniChar* sxContinueOp(sxPo sx)
{
  assert(sxIsContinue(sx));

  return sxIden(sxLhs(sx));
}

lxPo sxContinueArgs(sxPo sx)
{
  assert(sxIsContinue(sx));

  return sxBlockContent(sxRhs(sx));
}

// A call looks like
// call(<op>,{args})
sxPo sxCall(locationPo loc,uniChar *name,lxPo args)
{
  return sxBinary(loc,kwCall,mId(loc,name),sxBlock(loc,args));
}

logical sxIsCall(sxPo call)
{
  return sxIsBinary(call,kwCall) && sxIsIden(sxLhs(call)) && sxIsBlock(sxRhs(call));
}

uniChar *sxCallOp(sxPo call)
{
  assert(sxIsCall(call));

  return sxIden(sxLhs(call));
}

lxPo sxCallArgs(sxPo call)
{
  assert(sxIsCall(call));

  return sxBlockContent(sxRhs(call));
}

// A constructor looks like
// con(<op>,{args})
sxPo sxConstructor(locationPo loc,uniChar *name,lxPo args)
{
  return sxBinary(loc,kwConstructor,mId(loc,name),sxBlock(loc,args));
}

logical sxIsConstructor(sxPo call)
{
  return sxIsBinary(call,kwConstructor) && sxIsIden(sxLhs(call)) && sxIsBlock(sxRhs(call));
}

uniChar *sxConstructorOp(sxPo call)
{
  assert(sxIsConstructor(call));

  return sxIden(sxLhs(call));
}

lxPo sxConstructorArgs(sxPo call)
{
  assert(sxIsConstructor(call));

  return sxBlockContent(sxRhs(call));
}

logical sxIsArithExp(sxPo sx)
{
  return sxIsBinary(sx,kwPlus)
    || sxIsBinary(sx,kwMinus)
    || sxIsBinary(sx,kwTimes)
    || sxIsBinary(sx,kwDivide)
    || sxIsBinary(sx,kwRemainder)
    || sxIsBinary(sx,kwShiftLeft)
    || sxIsBinary(sx,kwShiftRight);
}

uniChar* sxArithOp(sxPo sx)
{
  assert(sxIsArithExp(sx));

  return sxIden(sxOp(sx));
}

sxPo sxArithLhs(sxPo sx)
{
  assert(sxIsArithExp(sx));
  return sxLhs(sx);
}

sxPo sxArithRhs(sxPo sx)
{
  assert(sxIsArithExp(sx));
  return sxRhs(sx);
}

logical sxIsCondition(sxPo sx)
{
  return sxIsBinary(sx,EqualName)
    || sxIsBinary(sx,NotEqualName)
    || sxIsBinary(sx,LessName)
    || sxIsBinary(sx,LessEqualName)
    || sxIsBinary(sx,LessEqualName)
    || sxIsBinary(sx,GreaterName)
    || sxIsBinary(sx,GreaterEqualName);
}

uniChar *sxConditionOp(sxPo sx)
{
  return sxIden(sxOp(sx));
}


sxPo sxUnary(locationPo loc,uniChar *name,sxPo arg)
{
  return mApply(loc,mId(loc,name),mCons(arg,nil));
}

logical sxIsUnry(sxPo sx)
{
  return sxIsApply(sx) && sxLength(sxArgs(sx))==1;
}

logical sxIsUnary(sxPo sx,uniChar *op)
{
  return sxIsUnry(sx) && sxIsIdentifier(sxOp(sx),op);
}

sxPo sxUnaryArg(sxPo sx)
{
  assert(sxIsUnry(sx));

  return sxArg(sx,0);
}

sxPo sxBinary(locationPo loc,uniChar *name,sxPo lhs,sxPo rhs)
{
  return mApply(loc,mId(loc,name),mCons(lhs,mCons(rhs,nil)));
}

logical sxIsBin(sxPo sx)
{
  return sxIsApply(sx) && sxLength(sxArgs(sx))==2;
}

logical sxIsBinary(sxPo sx,uniChar *op)
{
  return sxIsBin(sx) && sxIsIdentifier(sxOp(sx),op);
}

sxPo sxLhs(sxPo sx)
{
  assert(sxIsBin(sx));
  
  return sxArg(sx,0);
}

sxPo sxRhs(sxPo sx)
{
  assert(sxIsBin(sx));
  
  return sxArg(sx,1);
}

sxPo sxTernary(locationPo loc,uniChar *name,sxPo lhs,sxPo mdl,sxPo rhs)
{
  return mApply(loc,mId(loc,name),mCons(lhs,mCons(mdl,mCons(rhs,nil))));
}

logical sxIsTern(sxPo sx)
{
  return sxIsApply(sx) && sxLength(sxArgs(sx))==3;
}

logical sxIsTernary(sxPo sx,uniChar *name)
{
  return sxIsTern(sx) && sxIsIdentifier(sxOp(sx),name);
}

sxPo sxTernaryLhs(sxPo sx)
{
  assert(sxIsTern(sx));

  return sxArg(sx,0);
}

sxPo sxTernaryMdl(sxPo sx)
{
  assert(sxIsTern(sx));

  return sxArg(sx,1);
}

sxPo sxTernaryRhs(sxPo sx)
{
  assert(sxIsTern(sx));

  return sxArg(sx,2);
}

// nothing
sxPo sxNothing(locationPo loc)
{
  return mId(loc,kwNothing);
}

logical sxIsNothing(sxPo sx)
{
  return sxIsIdentifier(sx,kwNothing);
}

// A switch looks like:
// switch(<sel>,{<cases>})
sxPo sxSwitch(locationPo loc,sxPo sel,lxPo cases)
{
  return sxBinary(loc,kwSwitch,sel,sxBlock(loc,cases));
}

logical sxIsSwitch(sxPo sx)
{
  return sxIsBinary(sx,kwSwitch) && sxIsBlock(sxArg(sx,1));
}

sxPo sxSwitchSel(sxPo sx)
{
  assert(sxIsSwitch(sx));

  return sxLhs(sx);
}

lxPo sxSwitchCases(sxPo sx)
{
  assert(sxIsSwitch(sx));

  return sxBlockContent(sxRhs(sx));
}

// A case in a switch looks like:
// <ptn> -> <body>
logical sxIsCaseRule(sxPo sx)
{
  return sxIsBinary(sx,kwCase) || sxIsUnary(sx,kwDeflt);
}

sxPo sxCaseRule(locationPo loc,sxPo ptn,sxPo rhs)
{
  return sxBinary(loc,kwCase,ptn,rhs);
}

sxPo sxCasePtn(sxPo sx)
{
  assert(sxIsBinary(sx,kwCase));

  return sxLhs(sx);
}

sxPo sxCaseBody(sxPo sx)
{
  assert(sxIsBinary(sx,kwCase));

  return sxRhs(sx);
}

logical sxIsDefaultRule(sxPo rule)
{
  return sxIsUnary(rule,kwDeflt);
}

sxPo sxDefaultRule(locationPo loc,sxPo rhs)
{
  return sxUnary(loc,kwDeflt,rhs);
}

sxPo sxDefltBody(sxPo rule)
{
  assert(sxIsUnary(rule,kwDeflt));
  return sxUnaryArg(rule);
}

// A let bound expression/action looks like:
// let({<defs>}, <bound>)
sxPo sxLet(locationPo loc,lxPo defs,sxPo bound)
{
  return sxBinary(loc,kwLet,sxBlock(loc,defs),bound);
}

logical sxIsLet(sxPo sx)
{
  return sxIsBinary(sx,kwLet) && sxIsBlock(sxLhs(sx));
}

lxPo sxLetDefs(sxPo sx)
{
  assert(sxIsLet(sx));
  return sxBlockContent(sxLhs(sx));
}

sxPo sxLetBound(sxPo sx)
{
  assert(sxIsLet(sx));
  return sxRhs(sx);
}

// A Valof looks like
// valof({<actions>})
sxPo sxValof(locationPo loc,sxPo act)
{
  return sxUnary(loc,kwValof,act);
}

logical sxIsValof(sxPo sx)
{
  return sxIsUnary(sx,kwValof);
}

sxPo sxValofAction(sxPo sx)
{
  assert(sxIsValof(sx));

  return sxUnaryArg(sx);
}

// A valis looks like
// valis(<exp>)
sxPo sxValis(locationPo loc,sxPo exp)
{
  return sxUnary(loc,kwValis,exp);
}

logical sxIsValis(sxPo sx)
{
  return sxIsUnary(sx,kwValis);
}

sxPo sxValisExp(sxPo sx)
{
  assert(sxIsValis(sx));

  return sxArg(sx,0);
}

// An assert is assert <cond>
sxPo sxAssert(locationPo loc,sxPo exp)
{
  return sxUnary(loc,kwAssert,exp);
}

logical sxIsAssert(sxPo sx)
{
  return sxIsUnary(sx,kwAssert);
}

sxPo sxAssertCond(sxPo sx)
{
  assert(sxIsAssert(sx));

  return sxArg(sx,0);
}

sxPo sxBlock(locationPo loc,lxPo args)
{
  return mApply(loc,mId(loc,kwBlock),args);
}

logical sxIsBlock(sxPo sx)
{
  return sxIsApply(sx) && sxIsIdentifier(sxOp(sx),kwBlock);
}

lxPo sxBlockContent(sxPo sx)
{
  assert(sxIsBlock(sx));

  return sxArgs(sx);
}

// A conditional looks like
// if(<test>,<then>,<else>)

logical sxIsConditional(sxPo sx)
{
  return sxIsTernary(sx,kwIf);
}

sxPo sxConditional(locationPo loc,sxPo test,sxPo th,sxPo el)
{
  return sxTernary(loc,kwIf,test,th,el);
}

sxPo sxConditionalTest(sxPo sx)
{
  assert(sxIsConditional(sx));

  return sxArg(sx,0);
}

sxPo sxConditionalThen(sxPo sx)
{
  assert(sxIsConditional(sx));

  return sxArg(sx,1);
}

sxPo sxConditionalElse(sxPo sx)
{
  assert(sxIsConditional(sx));

  return sxArg(sx,2);
}

// A labeled action looks like:
// label :: action
sxPo sxLabeledAction(locationPo loc,uniChar *lbl,sxPo sx)
{
  return sxBinary(loc,kwColon,mId(loc,lbl),sx);
}

logical sxIsLabeled(sxPo sx)
{
  return sxIsBinary(sx,kwColon) && sxIsIden(sxLhs(sx));
}

uniChar * sxLabeledLabel(sxPo sx)
{
  assert(sxIsLabeled(sx));

  return sxIden(sxLhs(sx));
}

sxPo sxLabeledBody(sxPo sx)
{
  assert(sxIsLabeled(sx));

  return sxRhs(sx);
}

// A leave action looks like:
// leave(label)
logical sxIsLeaveAction(sxPo sx)
{
  return sxIsUnary(sx,kwLeave) && sxIsIden(sxUnaryArg(sx));
}

sxPo sxLeaveAction(locationPo loc,uniChar *lbl)
{
  return sxUnary(loc,kwLeave,mId(loc,lbl));
}

uniChar * sxLeaveLabel(sxPo sx)
{
  assert(sxIsLeaveAction(sx));

  return sxIden(sxUnaryArg(sx));
}

// A goto action looks like:
// goto(label)
sxPo sxGotoAction(locationPo loc,uniChar *lbl)
{
  return sxUnary(loc,kwGoto,mId(loc,lbl));
}

logical sxIsGotoAction(sxPo sx)
{
  return sxIsUnary(sx,kwGoto) && sxIsIden(sxUnaryArg(sx));
}

uniChar * sxGotoLabel(sxPo sx)
{
  assert(sxIsGotoAction(sx));

  return sxIden(sxUnaryArg(sx));
}

sxPo sxSync(locationPo loc,sxPo var,sxPo act)
{
  return sxBinary(loc,kwSync,var,act);
}

logical sxIsSync(sxPo sx)
{
  return sxIsBinary(sx,kwSync);
}

sxPo sxSyncVar(sxPo sx)
{
  assert(sxIsSync(sx));

  return sxLhs(sx);
}

sxPo sxSyncAct(sxPo sx)
{
  assert(sxIsSync(sx));

  return sxRhs(sx);
}

// A while loop looks like
// while(<test>,<action>)
sxPo sxWhileAction(locationPo loc,sxPo test,sxPo body)
{
  return sxBinary(loc,kwWhile,test,body);
}

logical sxIsWhile(sxPo sx)
{
  return sxIsBinary(sx,kwWhile);
}

sxPo sxWhileTest(sxPo sx)
{
  assert(sxIsWhile(sx));

  return sxLhs(sx);
}

sxPo sxWhileBody(sxPo sx)
{
  assert(sxIsWhile(sx));

  return sxRhs(sx);
}

logical sxIsLoopAction(sxPo sx)
{
  return sxIsUnary(sx,kwLoop);
}

sxPo sxLoopAction(locationPo loc,sxPo body)
{
  return sxUnary(loc,kwLoop,body);
}

sxPo sxLoopBody(sxPo loop)
{
  assert(sxIsLoopAction(loop));
  return sxUnaryArg(loop);
}

// A catch looks like
// try(<action>,{<catchers>})
sxPo sxCatch(locationPo loc,sxPo action,lxPo clauses)
{
  return sxBinary(loc,kwTry,action,sxBlock(loc,clauses));
}

logical sxIsCatch(sxPo sx)
{
  return sxIsBinary(sx,kwTry) && sxIsBlock(sxRhs(sx));
}

sxPo sxCatchBody(sxPo sx)
{
  assert(sxIsCatch(sx));
  return sxLhs(sx);
}

lxPo sxCatchClauses(sxPo sx)
{
  assert(sxIsCatch(sx));

  return sxBlockContent(sxRhs(sx));
}

// A throw looks like
// throw(<exp>)
sxPo sxThrowAction(locationPo loc,sxPo exp)
{
  return sxUnary(loc,kwThrow,exp);
}

logical sxIsThrowAction(sxPo sx)
{
  return sxIsUnary(sx,kwThrow);
}

sxPo sxThrowExp(sxPo sx)
{
  assert(sxIsThrowAction(sx));
  return sxUnaryArg(sx);
}

logical sxIsTypeVar(sxPo sx);

logical sxIsEnum(sxPo sx);

uniChar *sxEnumName(sxPo sx);

extern int yyparse(ioPo inFile, lxPo *result);

lxPo parseContent(uniChar *path)
{
  ioPo file = openURI(path, unknownEncoding);

  if(file!=Null){
    uniChar ch = inCh(file);		/* We skip over #! */

    if(ch=='#'){			/* look for standard #!/.... header */
      if((ch=inCh(file))=='!'){
	while((ch=inCh(file))!=uniEOF && ch!='\n')
	  ;			        /* consume the interpreter statement */
      }
      else{
	unGetChar(file,ch);
	unGetChar(file,'#');
      }
    }
    else
      unGetChar(file,ch);
    
    lxPo content = nil;
    int errors = yyparse(file,&content);
    
    closeFile(file);			/* close the source string file */
    if(!errors)
      return content;
    else{
      outMsg(logFile,"could not parse %U\n",path);
      return Null;
    }
  }
  else
    return Null;
}

static retCode dispMeta(ppDisplayPo disp,policyPo pol,sxPo m);

static retCode dispMetaSeq(ppDisplayPo disp, policyPo pol, lxPo lx, char *pre, char *sep, char *post)
{
  char *sp = "";
  retCode ret = ppAppend(disp,pol,pre);

  while(ret==Ok && lx!=nil){
    ppAppend(disp,pol,sp);
    sp = sep;
    ret = dispMeta(disp,pol,sxHead(lx));
    lx = sxTail(lx);
  }

  if(ret==Ok)
    ret = ppAppend(disp,pol,post);

  return ret;
}

static retCode dispMeta(ppDisplayPo disp,policyPo pol,sxPo m)
{
  if(sxIsTypeDef(m)){
    ppAppendU(disp,pol,kwType);
    ppAppend(disp,pol," ");
    dispMeta(disp,pol,sxTypeDefType(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwIs);
    ppAppend(disp,pol," ");
    DisplayPolicy elPol = { .indent=pol->indent+2 };
    
    return dispMetaSeq(disp,&elPol,sxTypeDefCons(m),"","\nor ","");
  }
  else if(sxIsConstructor(m)){
    ppAppendU(disp,pol,sxConstructorOp(m));
    return dispMetaSeq(disp,pol,sxConstructorArgs(m),"{",", ","}");
  }
  else if(sxIsImport(m)){
    ppAppendU(disp,pol,kwImport);
    return ppAppendId(disp,pol,sxImportPkg(m));
  }
  else if(sxIsFunction(m)){
    ppAppendU(disp,pol,kwFunction);
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,sxFunName(m));
    dispMetaSeq(disp,pol,sxFunArgs(m),"(",", ",")");
    ppAppendU(disp,pol,kwColon);
    dispMeta(disp,pol,sxFunType(m));
    ppAppend(disp,pol," => ");
    return dispMeta(disp,pol,sxFunExp(m));
  }
  else if(sxIsMemo(m)){
    ppAppendU(disp,pol,kwMemo);
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,sxMemoName(m));
    ppAppendU(disp,pol,kwColon);
    dispMeta(disp,pol,arrowResType(sxMemoType(m)));
    ppAppend(disp,pol," is ");
    return dispMeta(disp,pol,sxMemoExp(m));
  }
  else if(sxIsProcedure(m)){
    ppAppendU(disp,pol,kwProcedure);
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,sxProcName(m));
    dispMetaSeq(disp,pol,sxProcArgs(m),"(",", ",")");
    return dispMeta(disp,pol,sxProcBody(m));
  }
  else if(sxIsIsDeclaration(m)){
    ppAppendU(disp,pol,kwVar);
    ppAppend(disp,pol," ");
    dispMeta(disp,pol,sxDeclLval(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwIs);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxDeclValue(m));
  }
  else if(sxIsVarDeclaration(m)){
    ppAppendU(disp,pol,kwVar);
    ppAppend(disp,pol," ");
    dispMeta(disp,pol,sxDeclLval(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwAssign);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxDeclValue(m));
  }
  else if(sxIsAssignment(m)){
    dispMeta(disp,pol,sxAsgnLVal(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwAssign);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxAsgnRVal(m));
  }
  else if(sxIsCast(m)){
    dispMeta(disp,pol,sxCastExp(m));
    ppAppendU(disp,pol,kwColon);
    return dispMeta(disp,pol,sxCastType(m));
  }
  else if(sxIsCall(m)){
    ppAppendU(disp,pol,sxCallOp(m));
    return dispMetaSeq(disp,pol,sxCallArgs(m),"(",", ",")");
  }
  else if(sxIsContinue(m)){
    ppAppendU(disp,pol,kwContinue);
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,sxContinueOp(m));
    return dispMetaSeq(disp,pol,sxContinueArgs(m),"(",", ",")");
  }
  else if(sxIsArithExp(m)){
    sxPo lhs = sxArithLhs(m);
    if(sxIsArithExp(lhs)){
      ppAppend(disp,pol,"(");
      dispMeta(disp,pol,lhs);
      ppAppend(disp,pol,")");
    }
    else
      dispMeta(disp,pol,lhs);

    ppAppendU(disp,pol,sxArithOp(m));

    sxPo rhs = sxArithRhs(m);
    if(sxIsArithExp(rhs)){
      ppAppend(disp,pol,"(");
      dispMeta(disp,pol,rhs);
      return ppAppend(disp,pol,")");
    }
    else
      return dispMeta(disp,pol,rhs);
  }
  else if(sxIsCondition(m)){
    dispMeta(disp,pol,sxLhs(m));
    ppAppendU(disp,pol,sxIden(sxOp(m)));
    return dispMeta(disp,pol,sxRhs(m));
  }
  else if(sxIsBlock(m)){
    DisplayPolicy elPol = { .indent=pol->indent+2 };
    
    dispMetaSeq(disp,&elPol,sxBlockContent(m),"{\n",";\n","");
    return ppAppend(disp,pol,"\n}");
  }
  else if(sxIsSwitch(m)){
    ppAppendU(disp,pol,kwSwitch);
    ppAppend(disp,pol," ");
    dispMeta(disp,pol,sxSwitchSel(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwIn);
    
    DisplayPolicy elPol = { .indent=pol->indent+2 };
    
    dispMetaSeq(disp,&elPol,sxSwitchCases(m),"{\n",";\n","");
    return ppAppend(disp,pol,"\n}");
  }
  else if(sxIsCaseRule(m)){
    dispMeta(disp,pol,sxCasePtn(m));
    ppAppendU(disp,pol,kwCase);
    return dispMeta(disp,pol,sxCaseBody(m));
  }
  else if(sxIsDefaultRule(m)){
    ppAppendU(disp,pol,kwDeflt);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxDefltBody(m));
  }
  else if(sxIsConditional(m)){
    DisplayPolicy elPol = { .indent=pol->indent+2 };

    ppAppendU(disp,pol,kwIf);
    ppAppend(disp,pol," ");
    dispMeta(disp,pol,sxConditionalTest(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwThen);
    ppAppend(disp,&elPol,"\n");
    dispMeta(disp,&elPol,sxConditionalThen(m));
    ppAppend(disp,pol,"\n");
    ppAppendU(disp,pol,kwElse);
    ppAppend(disp,&elPol," ");
    return dispMeta(disp,pol,sxConditionalElse(m));
  }
  else if(sxIsSync(m)){
    ppAppendU(disp,pol,kwSync);
    dispMeta(disp,pol,sxSyncVar(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwDo);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxSyncAct(m));
  }
  else if(sxIsLet(m)){
    DisplayPolicy elPol = { .indent=pol->indent+2 };

    ppAppendU(disp,pol,kwLet);
    dispMetaSeq(disp,&elPol,sxLetDefs(m),"{\n",";\n","");
    ppAppend(disp,pol,"\n}");
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwIn);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxLetBound(m));
  }
  else if(sxIsValof(m)){
    ppAppendU(disp,pol,kwValof);
    return dispMeta(disp,pol,sxValofAction(m));
  }
  else if(sxIsValis(m)){
    ppAppendU(disp,pol,kwValis);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxValisExp(m));
  }
  else if(sxIsAssert(m)){
    ppAppendU(disp,pol,kwAssert);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxAssertCond(m));
  }
  else if(sxIsLabeled(m)){
    ppAppendU(disp,pol,sxLabeledLabel(m));
    ppAppendU(disp,pol,kwLabel);
    return dispMeta(disp,pol,sxLabeledBody(m));
  }
  else if(sxIsLeaveAction(m)){
    ppAppendU(disp,pol,kwLeave);
    ppAppend(disp,pol," ");
    return ppAppendU(disp,pol,sxLeaveLabel(m));
  }
  else if(sxIsGotoAction(m)){
    ppAppendU(disp,pol,kwGoto);
    ppAppend(disp,pol," ");
    return ppAppendU(disp,pol,sxGotoLabel(m));
  }
  else if(sxIsWhile(m)){
    ppAppendU(disp,pol,kwWhile);
    ppAppend(disp,pol," ");
    dispMeta(disp,pol,sxWhileTest(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwDo);
    return dispMeta(disp,pol,sxWhileBody(m));
  }
  else if(sxIsLoopAction(m)){
    ppAppendU(disp,pol,kwLoop);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxLoopBody(m));
  }
  else if(sxIsCatch(m)){
    ppAppendU(disp,pol,kwTry);
    ppAppend(disp,pol," ");
    dispMeta(disp,pol,sxCatchBody(m));
    ppAppend(disp,pol," ");
    ppAppendU(disp,pol,kwCatch);

    DisplayPolicy elPol = { .indent=pol->indent+2 };
    dispMetaSeq(disp,&elPol,sxCatchClauses(m),"{\n",";\n","");
    return ppAppend(disp,pol,"\n}");
  }
  else if(sxIsThrowAction(m)){
    ppAppendU(disp,pol,kwThrow);
    ppAppend(disp,pol," ");
    return dispMeta(disp,pol,sxThrowExp(m));
  }
  else if(isTypeVar(m)||isTypeFun(m)||isArrowType(m))
    return dispType(disp,pol,m);
  
  return dispSexp(disp,pol,m);
}

static retCode displayMeta(ioPo f,void *p,long width,long prec,logical alt)
{
  DisplayPolicy policy = { 0 };
  PPDisplay disp = { f, 0};

  return dispMeta(&disp,&policy,(sxPo)p);
}

retCode displaySexp(ioPo f,void *p,long width,long prec,logical alt)
{
  DisplayPolicy policy = { 0 };
  PPDisplay disp = { f, 0};
  return dispSexp(&disp,&policy,(sxPo)p);
}

retCode displaySeqExp(ioPo f,void *p,long width,long prec,logical alt)
{
  DisplayPolicy policy = { 0 };
  PPDisplay disp = initDisplay(f);
  if(alt)
    return dispMetaSeq(&disp,&policy,(lxPo)p,"",";\n","");
  else
    return dispSeq(&disp,&policy,(lxPo)p,"(",", ",")");
}

retCode pA(sxPo t)
{
  outMsg(logFile,"%A\n",t);
  flushOut();
  return Ok;
}

retCode pS(lxPo t)
{
  outMsg(logFile,"%#S\n",t);
  flushOut();
  return Ok;
}


retCode pM(sxPo m)
{
  outMsg(logFile,"%M\n",m);
  flushOut();
  return Ok;
}
