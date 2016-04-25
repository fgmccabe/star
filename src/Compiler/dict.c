/*
 * implementation of dictionary structure
 */
#include "compiler.h"
#include "dictP.h"
#include "compile.h"

#include <stdlib.h>
#include <ooio.h>
#include <formioP.h>

static poolPo dictPool = Null;
static poolPo varPool = Null;
static poolPo typePool = Null;
static poolPo fieldPool = Null;
static poolPo conPool = Null;
static poolPo undoPool = Null;

uniChar *ArithOverFlow, *ArithZeroDivide, 
  *TrueName, *FalseName, *ValofName, *CatchName,
  *MainName, *ProgramName, *TailName, *PkgDictVarName,
  *VoidName,
  *ColonName,
  *QuoteName,
  *EvalName,
  *NilName;

uniChar *ANONYMOUS;

uniChar *EqualName;
uniChar *NotEqualName;
uniChar *LessEqualName;
uniChar *LessName;
uniChar *GreaterEqualName;
uniChar *GreaterName;

uniChar *Nothing;

dictPo rootDict = Null;		       /* Cannot be initialized until codegen */

static retCode displayVar(ioPo f,void *p,long width,long prec,logical alt);
static dictStatePo pushUndo(uniChar *name,VarDefType entryType,dictPo dict);

void initDict()
{
  dictPool = newPool(sizeof(DictRecord),128);
  varPool = newPool(sizeof(VarInfoRec),1024);
  conPool = newPool(sizeof(ConstructorRecord),128);
  typePool = newPool(sizeof(TypeDefRecord),255);
  fieldPool = newPool(sizeof(FieldSpecRecord),255);
  undoPool = newPool(sizeof(DictUndoRec),1024);

  ArithOverFlow = mkInterned("overflow");
  ArithZeroDivide = mkInterned("zerodivide");
  TrueName = mkInterned("true");
  FalseName = mkInterned("false");
  ValofName = mkInterned("valof");
  CatchName = mkInterned("$catch$");
  TailName = mkInterned("$tail$");
  MainName = mkInterned("main");
  ProgramName = mkInterned("program");
  VoidName = mkInterned("void");
  ColonName = mkInterned(":");
  QuoteName = mkInterned("quote");
  EvalName = mkInterned("unquote");
  NilName = mkInterned("nil");
  PkgDictVarName = mkInterned("$dictVar$");
  Nothing = mkInterned("nothing");

  ANONYMOUS = mkInterned("_");

  EqualName = mkInterned("=");
  NotEqualName = mkInterned("!=");
  LessEqualName = mkInterned("<=");
  LessName = mkInterned("<");
  GreaterEqualName = mkInterned(">=");
  GreaterName = mkInterned(">");

  installMsgProc('V',displayVar);
}

void setRootDict(dictPo root)
{
  rootDict = root;
}

dictPo newDict()
{
  return funDict(0,Null);
}

static retCode copyConstructor(void *n,void *r,void *cl)
{
  conDefPo con = (conDefPo)r;
  hashPo constructors = (hashPo)cl;
  Install(con->name,con,constructors);

  return Ok;
}

static retCode copyType(void *n,void *r,void *cl)
{
  typeDefPo type = (typeDefPo)r;
  hashPo types = (hashPo)cl;
  Install(type->name,type->name,types);

  return Ok;
}

dictPo funDict(long varOffset,dictPo outer)
{
  dictPo dict = (dictPo)allocPool(dictPool);

  dict->vars = NewHash(63,(hashFun)uniHash,(compFun)uniCmp,NULL);
  dict->constructors = NewHash(16,(hashFun)uniHash, (compFun)uniCmp, Null);
  dict->types = NewHash(16,(hashFun)uniHash, (compFun)uniCmp, Null);
  dict->parent = outer;
  dict->nextLocal = varOffset;
  dict->localsSize = -varOffset;
  dict->freeSize = 0;
  dict->floats = 0;

  while(outer!=Null){
    ProcessTable(copyConstructor,outer->constructors,dict->constructors);
    ProcessTable(copyType,outer->types,dict->types);
    outer = outer->parent;
  }

  return dict;
}

dictPo parentDict(dictPo dict)
{
  return dict->parent;
}

static retCode mergeEntry(void *n,void *r,void *cl)
{
  dictPo dict = (dictPo)cl;
  varInfoPo var = (varInfoPo)r;

  declareVar(var,dict);
  return Ok;
}

static retCode mergeConstructor(void *n,void *r,void *cl)
{
  conDefPo def = (conDefPo)r;
  dictPo dict = (dictPo)cl;

  declareConstructor(def->name,def->conIx,def->maxIx,def->conSize,def->args,
		     def->type,def->lbl,dict);
  return Ok;
}

retCode mergeDict(dictPo dict,dictPo src)
{
  while(src!=Null){
    ProcessTable(mergeEntry,src->vars,dict);
    ProcessTable(mergeConstructor,src->constructors,dict);
    src = src->parent;
  }
  return Ok;
}

long localDepth(dictPo dict)
{
  return -ALIGN(dict->localsSize,16);
}

void setLocalsOffset(dictPo dict,long offset)
{
  dict->nextLocal = offset;
  dict->localsSize = -offset;
}

static retCode releaseVar(void *n,void *r,void *cl)
{
  varInfoPo var = (varInfoPo)r;
  freePool(varPool,var);
  return Ok;
}

varInfoPo search(uniChar *name,dictPo dict)
{
  return (varInfoPo)Search(name,dict->vars);
}

varInfoPo varReference(uniChar *name,dictPo dict)
{
  varInfoPo var = search(name,dict);

  if(var!=Null)
    return var;
  else if(parentDict(dict)!=Null){
    var = varReference(name,parentDict(dict));
    if(var!=Null){
      varInfoPo freeVar = reserve(vrLoc(var),name,vrInfType(var),
				  readOnly,False,vrInfKind(var),dict);
      Install(freeVar->name,freeVar,dict->vars); // do not generate an undo!
      return freeVar;
    }
    return var;
  }
  else
    return Null;
}

conDefPo findConstructor(uniChar *name,dictPo dict)
{
  while(dict!=Null){
    conDefPo con = (conDefPo)Search(name,dict->constructors);

    if(con!=Null)
      return con;
    dict = dict->parent;
  }
  return Null;
}

logical isConstructor(uniChar *name,dictPo dict)
{
  return findConstructor(name,dict)!=Null;
}

logical isEnumerated(uniChar *name,dictPo dict)
{
  conDefPo con = findConstructor(name,dict);
  if(con!=Null)
    return sxLength(con->args)==0;
  else
    return False;
}

lPo findEnumerated(uniChar *name,dictPo dict)
{
  conDefPo con = findConstructor(name,dict);
  assert(con!=Null && sxLength(con->args)==0);
  return con->lbl;
}

lxPo conDefArgs(conDefPo con)
{
  return con->args;
}

  
long freeSize(dictPo dict)
{
  return dict->freeSize;
}

void popDict(dictPo dict)
{
  ProcessTable(releaseVar,dict->vars,Null);
  DelHash(dict->vars);
  freePool(dictPool,dict);
}

static Register nextLocal(dictPo dict, sxPo type){
  Register reg = {lclReg, dict->nextLocal};
  dict->nextLocal += typeSize(type);
  return reg;
}

static Register nextFree(dictPo dict, sxPo type){
  if(dict->freeSize==0)
    dict->freeSize = POINTER_SIZE;
  Register reg = {freeReg, dict->freeSize};
  dict->freeSize+=typeSize(type);
  return reg;
}

varInfoPo reserve(locationPo loc,uniChar *name, sxPo type, rwMode access,
		  logical isLocal, sourceKind kind,dictPo dict)
{
  varInfoPo var = (varInfoPo)allocPool(varPool);

  var->name = uniIntern(name);
  var->loc = loc;
  var->type = type;
  var->access = access;
  var->where = basedVar;
  var->kind = kind;
  var->inited = False;
  if(isLocal)
    var->base = nextLocal(dict,type);
  else
    var -> base = nextFree(dict,type);

  return var;
}

void declareVar(varInfoPo info,dictPo dict)
{
  varInfoPo var = (varInfoPo)allocPool(varPool);

  *var = *info;
  
  Install(var->name,var,dict->vars);
  pushUndo(var->name,VarDef,dict);
}

static Register localRg(int off){
  Register reg = {lclReg, off};
  return reg;
}

static Register freeRg(int off){
  Register reg = {freeReg, off};
  return reg;
}

varInfoPo declare(locationPo loc,uniChar *name, sxPo type, rwMode access,
		  logical isLocal, sourceKind kind,int offset,
		  dictPo dict)
{
  varInfoPo var = (varInfoPo)allocPool(varPool);

  var->name = uniIntern(name);
  var->loc = loc;
  var->type = type;
  var->access = access;

  if(isLocal)
    var->base = localRg(offset);
  else
    var->base = freeRg(offset);

  var->where = basedVar;
  var->kind = kind;
  var->inited = False;

  Install(var->name,var,dict->vars);
  pushUndo(var->name,VarDef,dict);
  return var;
}

void declareLit(locationPo loc,uniChar *name,uniChar *str,
		sxPo type,sourceKind kind,dictPo dict)
{
  varInfoPo var = (varInfoPo)allocPool(varPool);

  var->name = uniIntern(name);
  var->loc = loc;
  var->type = type;
  var->access = readOnly;
  var->kind = kind;
  var->where = literal;
  var->l.str = str;
  var->inited = True;

  Install(var->name,var,dict->vars);
  pushUndo(var->name,VarDef,dict);
}

void declareInfo(varInfoPo info,uniChar *str,dictPo dict)
{
  varInfoPo var = (varInfoPo)allocPool(varPool);
  var->name = info->name;
  var->loc = info->loc;
  var->type = info->type;
  var->access = readOnly;
  var->kind = info->kind;
  var->where = literal;
  var->l.str = str;
  var->inited = True;
  
  Install(var->name,var,dict->vars);
  pushUndo(var->name,VarDef,dict);
}

uniChar* vrInfName(varInfoPo var)
{
  return var->name;
}

varSource vrInfSource(varInfoPo var)
{
  return var->where;
}

sourceKind vrInfKind(varInfoPo var)
{
  return var->kind;
}

logical isVrLocal(varInfoPo var)
{
  return var->where==basedVar && var->base.regCl==lclReg;
}

logical isVrFree(varInfoPo var)
{
  return var->where==basedVar &&  var->base.regCl==freeReg;
}

int vrInfOffset(varInfoPo var)
{
  return var->l.off;
}

sxPo vrInfType(varInfoPo var)
{
  return var->type;
}

locationPo vrLoc(varInfoPo var)
{
  return var->loc;
}

logical isInited(varInfoPo var)
{
  return var->inited;
}

static dictStatePo pushUndo(uniChar *name,VarDefType entryType,dictPo dict)
{
  dictStatePo undo = (dictStatePo)allocPool(undoPool);
  undo->name = name;
  undo->entryType = entryType;
  undo->prev = dict->undoState;
  undo->dict = dict; // sanity check
  dict->undoState = undo;
  return undo;
}

dictStatePo dictState(dictPo dict)
{
  return dict->undoState;
}

void resetDict(dictPo dict,dictStatePo mark)
{
  while(dict->undoState!=mark){
    dictStatePo undo = dict->undoState;
    assert(undo->dict==dict);
    dict->undoState = undo->prev;

    switch(undo->entryType){
    case ConstructorDef:{
      conDefPo con = (conDefPo)Search(undo->name,dict->constructors);
      assert(con!=Null);
      freePool(conPool,con);
      Uninstall(undo->name,dict->constructors);
      break;
    }
    case VarDef:{
      varInfoPo var = (varInfoPo)Search(undo->name,dict->vars);
      assert(var!=Null);
      freePool(varPool,var);
      Uninstall(undo->name,dict->vars);
      break;
    }
    default:
      assert(False);
    }
    freePool(undoPool,undo);
  }
}

typeDefPo declareType(uniChar *name,sxPo tp,dictPo dict)
{
  typeDefPo type = (typeDefPo)allocPool(typePool);

  type->name = uniIntern(name);
  type->typeSpec = tp;
  type->fields = NewHash(7,(hashFun)uniHash,(compFun)uniCmp,NULL);
  hashPut(dict->types,type->name,type);
  return type;
}

typeDefPo findType(uniChar *name,dictPo dict)
{
  return (typeDefPo)hashGet(dict->types,name);
}

fieldPo findFieldSpec(uniChar *name,typeDefPo desc)
{
  return (fieldPo)hashGet(desc->fields,name);
}

fieldPo declareField(uniChar *name,sxPo fieldType,typeDefPo type)
{
  fieldPo field = (fieldPo)allocPool(fieldPool);
  field->name = uniIntern(name);
  field->type = fieldType;
  hashPut(type->fields,field->name,field);
  return field;
}

conDefPo declareConstructor(uniChar *name, int conIx, int maxIx,
			    long conSize, lxPo args, sxPo type, lPo lbl,
			    dictPo dict)
{
  conDefPo con = (conDefPo)allocPool(conPool);
  con->name = uniIntern(name);
  con->conIx = conIx;
  con->maxIx = maxIx;
  con->conSize = conSize;
  con->args = args;
  con->type = type;
  con->lbl = lbl;
  Install(con->name,con,dict->constructors);
  pushUndo(con->name,ConstructorDef,dict);
  return con;
}

retCode processDict(dictPo dict,dictProc proc,void *cl)
{
  return ProcessTable((procFun)proc,dict->vars,cl);
}

char *sourceName(varSource where)
{
  switch(where){
  case basedVar:
    return "basedVar";
  case literal:
    return "literal";
  case label:
    return "label";
  case registr:
    return "register";
  default:
    return "invalid source";
  }
}

char *kindName(sourceKind kind)
{
  switch(kind){
  case general:
    return "general";
  case rawChar:
    return "rawChar";
  case rawInt:
    return "rawInt";
  case rawLong:
    return "rawLong";
  case rawFloat:
    return "rawFloat";
  default:
    return "invalid kind";
  }
}

char *accessName(rwMode access)
{
  switch(access){
  case readOnly:
    return "RO";
  case readWrite:
    return "RW";
  default:
    return "invalid access";
  }
}

static retCode showReg(ioPo f,Register reg,char *cont){
  switch(reg.regCl){
    case argReg:
      return outMsg(f,"A[%d]%s",reg.regNo,cont);
    case lclReg:
      return outMsg(f,"L[%d]%s",reg.regNo,cont);
    case freeReg:
      return outMsg(f,"F[%d]%s",reg.regNo,cont);
  }
}

retCode dVar(ioPo f,varInfoPo var)
{
  outMsg(f,"%U[%s]:%T->",var->name,accessName(var->access),var->type);
  switch(var->where){
  case registr:
    return showReg(f,var->base,"\n");
  case literal:
    return outMsg(f,"%B\n",var->l.lit);
  case label:
    return outMsg(f,"B\n",var->l.lit);
  case basedVar:
    showReg(f,var->base,"");
    return outMsg(f,".%d\n",var->l.off);
  default:
    return outMsg(f,"??\n",var->name);
  }
}

void dV(varInfoPo var)
{
  dVar(logFile,var);
  flushOut();
}

static retCode dEntry(void *n,void *r,void *cl)
{
  return dVar((ioPo)cl,(varInfoPo)r);
}

static retCode dConstructor(void *n,void *r,void *cl)
{
  conDefPo def = (conDefPo)r;
  ioPo io = (ioPo)cl;

  return outMsg(io,"%U[%d/%d]:%T @ %B\n",
		def->name,def->conIx,def->maxIx,def->type,def->lbl);
}

void dDict(dictPo dict)
{
  while(dict!=Null){
    outMsg(logFile,"localsSize=%d, nextLocal=%d, freeSize=%d\n",
	   dict->localsSize,dict->nextLocal,dict->freeSize);
    ProcessTable(dEntry,dict->vars,logFile);
    ProcessTable(dConstructor,dict->constructors,logFile);
    dict = dict->parent;
    if(dict!=Null)
      outMsg(logFile,"+++++\n");
  }
  outMsg(logFile,"-----\n");
  flushOut();
}

static retCode displayVar(ioPo f,void *p,long width,long prec,logical alt)
{
  varInfoPo var = (varInfoPo)p;
  return dVar(f,var);
}

retCode pV(varInfoPo var)
{
  outMsg(logFile,"%V\n",var);
  flushOut();
  return Ok;
}
