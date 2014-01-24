/*
 * Handle types in the compiler and the system
 */

#include "compiler.h"
#include "meta.h"
#include "compile.h"
#include "type.h"
#include <ooio.h>
#include <formioP.h>
#include <stdlib.h>
#include "pp.h"

uniChar *rawIntType;
uniChar *rawLongType;
uniChar *rawFloatType;
uniChar *rawCharType;
uniChar *rawStringType;
uniChar *rawFileType;
uniChar *rawErrorType;

uniChar *TYPE_VAR;
uniChar *ARROW_TYPE;
uniChar *PTN_TYPE;
uniChar *TYPE_EXP;

uniChar *VOID_TYPE;
uniChar *DICT_TYPE;
uniChar *BOOL_TYPE;

sxPo voidType,intType,longType,floatType,charType,stringType,fileCafeType,
  errorType,dictType,booleanType;

sxPo voidCon;
sxPo voidSpec;

static retCode displayType(ioPo f,void *p,long width,long prec,logical alt);

typedef struct _reset_record_ *bindPo;

typedef struct _reset_record_ {
  sxPo value;
  hashPo env;
} ResetRec;

static poolPo bindPool;

void initTypes()
{
  VOID_TYPE = mkInterned("void");
  DICT_TYPE = mkInterned("$dict$");
  BOOL_TYPE = mkInterned("boolean");
    
  rawIntType = mkInterned("#int");
  rawLongType = mkInterned("#long");
  rawFloatType = mkInterned("#float");
  rawCharType = mkInterned("#char");
  rawStringType = mkInterned("#string");
  rawFileType = mkInterned("#file");
  rawErrorType = mkInterned("#error");

  ARROW_TYPE = mkInterned("=>");
  PTN_TYPE = mkInterned("<=");
  TYPE_VAR = mkInterned("%tvar");
  TYPE_EXP = mkInterned("%type");
  
  installMsgProc('T',displayType);	  /* extend outMsg with types */
    
  voidType = mId(Null,VOID_TYPE);
  intType = mId(Null,rawIntType);
  longType = mId(Null,rawLongType);
  floatType = mId(Null,rawFloatType);
  charType = mId(Null,rawCharType);
  stringType = mId(Null,rawStringType);
  fileCafeType = mId(Null,rawFileType);
  errorType = mId(Null,rawErrorType);
  dictType = mId(Null,DICT_TYPE);
  booleanType = mId(Null,BOOL_TYPE);

  bindPool = newPool(sizeof(ResetRec),32);
}

logical isTypeVar(sxPo type)
{
  return sxIsUnary(type, TYPE_VAR);
}

logical isRawCharType(sxPo type)
{
  return sxIsIdentifier(type,rawCharType);
}

logical isRawIntType(sxPo type)
{
  return sxIsIdentifier(type,rawIntType);
}

logical isRawLongType(sxPo type)
{
  return sxIsIdentifier(type,rawLongType);
}

logical isRawFloatType(sxPo type)
{
  return sxIsIdentifier(type,rawFloatType);
}

logical isRawStringType(sxPo type)
{
  return sxIsIdentifier(type,rawStringType);
}

logical isRawFileType(sxPo type)
{
  return sxIsIdentifier(type,rawFileType);
}

logical isVoidType(sxPo type)
{
  return sxIsIdentifier(type,VOID_TYPE);
}

logical isRawType(sxPo type)
{
  return isRawIntType(type) || isRawLongType(type) || isRawFloatType(type)
    || isRawCharType(type) || isRawStringType(type) || isVoidType(type)
    || isRawFileType(type);
}

TypeKind tpKind(sxPo type)
{
  if(isTypeVar(type))
    return TypeVar;
  else if(isArrowType(type))
    return ArrowType;
  else
    return TypeExp;
}


// A type variable is represented using
// %var(<name>)

sxPo sxTypeVar(locationPo loc,uniChar *name)
{
  return sxUnary(loc,TYPE_VAR,mId(loc,name));
}

uniChar *tpVarName(sxPo t)
{
  assert(isTypeVar(t));
  return sxIden(sxUnaryArg(t));
}

// A type expression is represented using
// %type(<name>,{<args>})
sxPo sxTypeExp(locationPo loc,uniChar *name, lxPo args)
{
  assert(name!=Null && args!=Null);

  return sxBinary(loc,TYPE_EXP,mId(loc,name),sxBlock(loc,args));
}

sxPo sxTypeFun(locationPo loc,sxPo op,lxPo args)
{
  return sxBinary(loc,TYPE_EXP,op,sxBlock(loc,args));
}

logical isTypeFun(sxPo t)
{
  return sxIsBinary(t,TYPE_EXP);
}

logical isTypeExp(sxPo t)
{
  return isTypeFun(t) && sxIsIden(sxLhs(t));
}

uniChar *typeExpName(sxPo tp)
{
  assert(isTypeExp(tp));
  return sxIden(sxLhs(tp));
}

sxPo sxTypeOp(sxPo tp)
{
  assert(isTypeExp(tp));
  return sxLhs(tp);
}

lxPo sxTypeArgs(sxPo tp)
{
  assert(isTypeExp(tp));
  return sxBlockContent(sxRhs(tp));
}

sxPo sxArrowType(locationPo loc,lxPo args, sxPo resType)
{
  return sxBinary(loc,ARROW_TYPE,sxBlock(loc,args),resType);
}

logical isArrowType(sxPo type)
{
  return sxIsBinary(type,ARROW_TYPE) && sxIsBlock(sxLhs(type));
}

lxPo arrowArgTypes(sxPo type)
{
  assert(isArrowType(type));
  return sxBlockContent(sxLhs(type));
}

sxPo arrowtypeArg(sxPo type,int ix)
{
  assert(isArrowType(type));
  return sxEl(sxBlockContent(sxLhs(type)),ix);
}

sxPo arrowResType(sxPo type)
{
  assert(isArrowType(type));

  return sxRhs(type);
}

sxPo sxPttrnType(locationPo loc,lxPo args, sxPo ptnType)
{
  return sxBinary(loc,PTN_TYPE,sxBlock(loc,args),ptnType);
}

logical isPttrnType(sxPo type)
{
  return sxIsBinary(type,PTN_TYPE) && sxIsBlock(sxLhs(type));
}

lxPo ptnArgTypes(sxPo type)
{
  assert(isPttrnType(type));
  return sxBlockContent(sxLhs(type));
}

sxPo ptnTypeArg(sxPo type,int ix)
{
  assert(isPttrnType(type));
  return sxEl(sxBlockContent(sxLhs(type)),ix);
}

sxPo ptnPtnType(sxPo type)
{
  assert(isPttrnType(type));

  return sxRhs(type);
}

static retCode sameType(sxPo t1,hashPo r1,sxPo t2,hashPo r2,locationPo loc);

retCode checkType(sxPo actual, sxPo *expected,locationPo loc)
{
  if(*expected==Null){
    *expected = actual;
    return Ok;
  }
  else{
    hashPo r1 = NewHash(15,(hashFun)uniHash,(compFun)uniCmp,Null);
    hashPo r2 = NewHash(15,(hashFun)uniHash,(compFun)uniCmp,Null);
    retCode ret = sameType(actual,r1,*expected,r2,loc);

    DelHash(r1);
    DelHash(r2);			/* discard the bindings */
    
    return ret;
  }
}

static retCode sameTypes(lxPo t1,hashPo r1,lxPo t2,hashPo r2,locationPo loc)
{
  if(t1==nil && t2==nil)
    return Ok;
  else if(sameType(sxHead(t1),r1,sxHead(t2),r2,loc)==Ok)
    return sameTypes(sxTail(t1),r1,sxTail(t2),r2,loc);
  else
    return Fail;
}

static void deRef(sxPo *t,hashPo *e)
{
  sxPo tp = *t;
  hashPo env = *e;
  while(isTypeVar(tp)){
    bindPo binding = Search(tpVarName(tp),env);
    if(binding!=Null){
      tp = binding->value;
      env = binding->env;
    }
    else{
      *t = tp;
      *e = env;
    }
  }
}

static logical occursCheck(uniChar *v,hashPo vE,sxPo tp,hashPo e);

static logical occursCheckArgs(uniChar *v,hashPo vE,lxPo tps,hashPo e)
{
  while(tps!=nil){
    if(occursCheck(v,vE,sxHead(tps),e))
      return True;
    else
      tps = sxTail(tps);
  }
  return False;
}

static logical occursCheck(uniChar *v,hashPo vE,sxPo tp,hashPo e)
{
  deRef(&tp,&e);
  if(isTypeVar(tp)){
    if(vE==e && uniCmp(tpVarName(tp),v)==0)
      return True;
    else
      return False;
  }
  else if(isTypeExp(tp)){
    if(occursCheck(v,vE,sxTypeOp(tp),e))
      return True;
    else
      return occursCheckArgs(v,vE,sxTypeArgs(tp),e);
  }
  else if(isArrowType(tp)){
    if(occursCheckArgs(v,vE,arrowArgTypes(tp),e))
      return True;
    else
      return occursCheck(v,vE,arrowResType(tp),e);
  }
  else
    return False;
}

static retCode bind(uniChar *v,hashPo vE,sxPo tp,hashPo e,locationPo loc)
{
  if(isRawType(tp)){
    reportError(loc,"may not bind to a raw type: %T",tp);
    return Fail;
  }

  if(!occursCheck(v,vE,tp,e)){
    bindPo binding = allocPool(bindPool);
    binding->value = tp;
    binding->env = e;
    Install(v,binding,vE);
    return Ok;
  }
  else
    return Fail;
}


retCode sameType(sxPo t1,hashPo r1,sxPo t2,hashPo r2,locationPo loc)
{
  deRef(&t1,&r1);
  deRef(&t2,&r2);
  if(isTypeVar(t1)){
    if(isTypeVar(t2)){
      if(r1==r2 && uniCmp(tpVarName(t1),tpVarName(t2))==0)
	return Ok;
      else
	return bind(tpVarName(t1),r1,t2,r2,loc);
    }
    else
      return bind(tpVarName(t1),r1,t2,r2,loc);
  }
  else if(isTypeVar(t2))
    return bind(tpVarName(t2),r2,t1,r1,loc);
  else if(sxIsIden(t1)){
    if(sxIsIden(t2)){
      if(uniCmp(sxIden(t1),sxIden(t2))==0)
	return Ok;
      else
	return Fail;
    }
    else
      return Fail;
  }
  else if(isTypeExp(t1)){
    if(isTypeExp(t2)){
      if(sameType(sxTypeOp(t1),r1,sxTypeOp(t2),r2,loc)==Ok)
	return sameTypes(sxTypeArgs(t1),r1,sxTypeArgs(t2),r2,loc);
      else
	return Fail;
    }
    else
      return Fail;
  }
  else if(isArrowType(t1)){
    if(isArrowType(t2)){
      if(sameTypes(arrowArgTypes(t1),r1,arrowArgTypes(t2),r2,loc)==Ok)
	return sameType(arrowResType(t1),r1,arrowResType(t2),r2,loc);
      else
	return Fail;
    }
    else
      return Fail;
  }
  else
    return Fail;
}

static retCode dispTypes(ppDisplayPo disp,policyPo pol,lxPo t,char *l,char *r)
{
  retCode ret = ppAppend(disp,pol,l);
  char *sep = "";

  while(ret==Ok && t!=nil){
    ret = ppAppend(disp,pol,sep);
    sep = ", ";

    ret = dispType(disp,pol,sxHead(t));
    t = sxTail(t);
  }
  ppAppend(disp,pol,r);
  if(t!=nil)
    ret = Error;
  return ret;
}

retCode dispType(ppDisplayPo disp,policyPo pol,sxPo t)
{
  if(isTypeVar(t)){
    ppAppend(disp,pol,"%");
    return ppAppendU(disp,pol,tpVarName(t));
  }
  else if(sxIsIden(t))
    return ppAppendId(disp,pol,sxIden(t));
  else if(isTypeFun(t)){
    dispType(disp,pol,sxTypeOp(t));
    return dispTypes(disp,pol,sxTypeArgs(t),"<",">");
  }
  else if(isPttrnType(t)){
    dispType(disp,pol,ptnPtnType(t));
    ppAppend(disp,pol,"<=");
    ppAppend(disp,pol,"(");
    dispTypes(disp,pol,ptnArgTypes(t),"(",")");
    return ppAppend(disp,pol,")");
  }
  else if(isArrowType(t)){
    ppAppend(disp,pol,"(");
    dispTypes(disp,pol,arrowArgTypes(t),"(",")");
    ppAppend(disp,pol,"=>");
    dispType(disp,pol,arrowResType(t));
    return ppAppend(disp,pol,")");
  }
  else
    return Error;
}

retCode displayType(ioPo f,void *p,long width,long prec,logical alt)
{
  DisplayPolicy policy = { 0 };
  PPDisplay disp = { f, 0};
  sxPo tp = (sxPo)p;

  if(tp==Null)
    return ppAppend(&disp,&policy,"(null-type)");
  else
    return dispType(&disp,&policy,tp);
}

void dT(sxPo t)
{
  displayType(logFile,t,0,0,False);
  outMsg(logFile,"\n");
  flushOut();
}


