/*
 * Manage S-expressions, allocate space and provide accessor functions
 *
 * The sexp type is defined in way that should promote interoperability between
 * C code and Cafe code.
 * 
 */
#include <ooio.h>
#include <assert.h>
#include <stdlib.h>
#include <formioP.h>

#include "compiler.h"
#include "dict.h"
#include "sexpP.h"
#include "Headers/locationP.h"

static poolPo sxPool = Null;
static poolPo lxPool = Null;

lxPo nil;

void initSexpressions()
{
  sxPool = newPool(sizeof(SexpObject),4096);
  lxPool = newPool(sizeof(SequenceObj),4096);

  nil = mCons(Null,Null);		/* special end of sequence marker */
  initLocation();
}

sxPo mId(locationPo loc,char *name)
{
  sxPo trm = (sxPo)allocPool(sxPool);
  trm->loc = loc;
  trm->con = symbCon;
  trm->S.text = uniIntern(name);
  return trm;
}

sxPo mChar(locationPo loc,char ch)
{
  sxPo trm = (sxPo)allocPool(sxPool);
  trm->loc = loc;
  trm->con = charCon;
  trm->S.ch = ch;
  return trm;
}

sxPo mStr(locationPo loc,char *str)
{
  sxPo trm = (sxPo)allocPool(sxPool);
  trm->loc = loc;
  trm->con = stringCon;
  trm->S.text = uniIntern(str);
  return trm;
}

sxPo mInt(locationPo loc,int i)
{
  sxPo trm = (sxPo)allocPool(sxPool);
  trm->loc = loc;
  trm->con = intCon;
  trm->S.ix = i;
  return trm;
}

sxPo mLong(locationPo loc,integer i)
{
  sxPo trm = (sxPo)allocPool(sxPool);
  trm->loc = loc;
  trm->con = integerCon;
  trm->S.ix = i;
  return trm;
}

sxPo mFloat(locationPo loc,double d)
{
  sxPo trm = (sxPo)allocPool(sxPool);
  trm->loc = loc;
  trm->con = floatCon;
  trm->S.d = d;
  return trm;
}

sxPo mApply(locationPo loc,sxPo op,lxPo args)
{
  sxPo trm = (sxPo)allocPool(sxPool);
  trm->loc = loc;
  trm->con = applyCon;
  trm->S.app.op = op;
  trm->S.app.args = args;
  return trm;
}

lxPo sxApplyArgs(sxPo exp)
{
  assert(sxIsApply(exp));

  return exp->S.app.args;
}

lxPo mCons(sxPo head,lxPo tail)
{
  lxPo trm = (lxPo)allocPool(lxPool);
  trm->head = head;
  trm->tail = tail;
  return trm;
}

logical sxIsApply(sxPo sx)
{
  return (logical) (sx->con == applyCon);
}

logical sxIsIden(sxPo sx)
{
  return (logical) (sx->con == symbCon);
}

logical sxIsChar(sxPo sx)
{
  (logical)(return sx->con==charCon);
}

logical sxIsStr(sxPo sx)
{
  return (logical) (sx->con == stringCon);
}

logical sxIsInt(sxPo sx)
{
  return (logical) (sx->con == intCon);
}

logical sxIsLong(sxPo sx)
{
  return (logical) (sx->con == integerCon);
}

logical sxIsFloat(sxPo sx)
{
  return (logical) (sx->con == floatCon);
}

locationPo sxLoc(sxPo sx)
{
  return sx->loc;
}

char sxChar(sxPo sx)
{
  assert(sxIsChar(sx));

  return sx->S.ch;
}

integer sxInt(sxPo sx)
{
  assert(sxIsInt(sx));
  return sx->S.ix;
}
  
integer sxLong(sxPo sx)
{
  assert(sxIsLong(sx));
  return sx->S.ix;
}
  
double sxFloat(sxPo sx)
{
  assert(sxIsFloat(sx));
  return sx->S.d;
}

char* sxIden(sxPo sx)
{
  assert(sxIsIden(sx));

  return sx->S.text;
}

char* sxText(sxPo sx)
{
  assert(sxIsStr(sx));

  return sx->S.text;
}
  
sxPo sxApply(locationPo loc,char *name,lxPo args)
{
  sxPo op = mId(loc,name);
  return mApply(loc,op,args);
}

char *sxApplyOp(sxPo sx)
{
  assert(sxIsApply(sx));
  return sxIden(sxOp(sx));
}

sxPo sxOp(sxPo sx)
{
  assert(sxIsApply(sx));

  return sx->S.app.op;
}

lxPo sxArgs(sxPo sx)
{
  assert(sxIsApply(sx));

  return sx->S.app.args;
}

sxPo sxArg(sxPo sx,int ix)
{
  lxPo args = sxArgs(sx);

  assert(sxLength(args)>ix);
  return sxEl(args,ix);
}

sxPo sxHead(lxPo lx)
{
  assert(lx!=nil);
  return lx->head;
}
  
lxPo sxTail(lxPo sx)
{
  return sx->tail;
}

lxPo setTail(lxPo lx,lxPo tail)
{
  assert(lx!=nil);

  lx->tail = tail;
  return lx;
}

sxPo sxEl(lxPo sq,int ix)
{
  while(ix>0){
    assert(sq!=nil);
    sq = sxTail(sq);
    ix--;
  }
  return sq->head;
}

int sxLength(lxPo sx)
{
  int count = 0;

  while(sx!=nil){
    count++;
    sx = sxTail(sx);
  }
  return count;
}

lxPo addToSequence(lxPo list,sxPo el)
{
  if(list==nil)
    return mCons(el,list);
  else
    return setTail(list,addToSequence(sxTail(list),el));
}

static lxPo sxLst(locationPo loc,va_list args)
{
  sxPo el = (sxPo)va_arg(args,sxPo);

  if(el==Null)
    return nil;
  else{
    lxPo rest = sxLst(loc,args);
    return mCons(el,rest);
  }
}

lxPo sxList(locationPo loc,...)
{
  va_list args;
  va_start(args,loc);

  lxPo list = sxLst(loc,args);

  va_end(args);
  return list;
}

sxPo sxTerm(locationPo loc,char *op,...)
{
  va_list args;
  va_start(args,op);

  lxPo list = sxLst(loc,args);

  va_end(args);
  return mApply(loc,mId(loc,op),list);
}

retCode dispSexp(ppDisplayPo disp, policyPo pol, sxPo sx)
{
  switch(sx->con){
  case applyCon:{
    DisplayPolicy elPol = { .indent=pol->indent+2 };
    dispSexp(disp,pol,sxOp(sx));
    return dispSeq(disp,&elPol,sxArgs(sx),"(",", ",")");
  }
  case intCon:
    return ppAppendI(disp,pol,sxInt(sx));
  case integerCon:
    return ppAppendI(disp,pol,sxLong(sx));
  case floatCon:
    return ppAppendF(disp,pol,sxFloat(sx));
  case stringCon:
    return ppAppendQ(disp,pol,sxText(sx));
  case charCon:
    return ppAppendC(disp,pol,sxChar(sx));
  case symbCon:
    return ppAppendId(disp,pol,sxIden(sx));
  default:
    outMsg(logFile,"invalid S-expression to print\n");
    return Error;
  }
}

retCode dispSeq(ppDisplayPo disp, policyPo pol, lxPo lx, char *pre, char *sep, char *post)
{
  char *sp = "";
  retCode ret = ppAppend(disp,pol,pre);

  while(ret==Ok && lx!=nil){
    ppAppend(disp,pol,sp);
    sp = sep;
    ret = dispSexp(disp,pol,sxHead(lx));
    lx = sxTail(lx);
  }

  if(ret==Ok)
    ret = ppAppend(disp,pol,post);

  return ret;
}


