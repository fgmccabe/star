/*
 * Display an assembled fragment
 */
#include "assemP.h"
#include "cafeOptions.h"
#include "utils.h"

#include <ooio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "formioP.h"

static retCode disp_tos(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  return outMsg(f,"%s\n",op);
}

static retCode disp_nOp(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  return outMsg(f,"%s\n",op);
}

static retCode disp_i32(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  return outMsg(f,"%s %d\n",op,ins->i);
}

static retCode disp_arg(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  return outMsg(f,"%s a[%d]\n",op,ins->i);
}

static retCode disp_lcl(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  return outMsg(f,"%s l[%d]\n",op,ins->i);
}

static retCode disp_env(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  return outMsg(f,"%s e[%d]\n",op,ins->i);
}

static retCode disp_off(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  return outMsg(f,"%s %B\n",op, ins->lbl);
}

static retCode showConstant(ioPo f,mtdPo mtd,int64 ix)
{
  constPo con = poolConstant(mtd,ix);
  if(con!=Null)
    return con->show(f,con);
  else
    return outMsg(f," <bad constant %d>",ix);
}

static retCode disp_lit(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  tryRet(outMsg(f,"%s ",op));
  return showConstant(f,mtd,ins->i);
}

static retCode disp_Es(ioPo f,mtdPo mtd,assemInsPo ins,char *op)
{
  tryRet(outMsg(f,"%s ",op));
  return showConstant(f,mtd,ins->i);
}

retCode dumpIns(ioPo f,mtdPo mtd,assemInsPo ins)
{
  outMsg(f,"0x%x: ",ins->pc);

  switch(ins->op){
#undef instruction

#define instruction(Op,A1,Cmt) \
    case Op:\
      return disp_##A1(f,mtd,ins,#Op);

#include "instructions.h"
  case label:
    return outMsg(f,"%B:\n",ins->lbl);

  case frame:
    tryRet(outMsg(f,"frame "));
    tryRet(showConstant(f,mtd,ins->i));
    return outMsg(f,"\n");

  default:
    return Error;
  }
}

static retCode showLocal(ioPo o,mtdPo mtd,localVarPo lcl)
{
  constPo c = poolConstant(mtd,lcl->name);
  if(c!=Null){
    char *name = c->value.txt;

    c = poolConstant(mtd,lcl->sig);

    if(c!=Null){
      char *sig = c->value.txt;

      outMsg(o,"%U:%U [%d]",name,sig,lcl->off);

      if(lcl->from->pc!=Null && lcl->to->pc!=Null)
      	return outMsg(o," (0x%x-0x%x)\n",lcl->from->pc->pc,lcl->to->pc->pc);
      else
      	return outMsg(o,"\n");
    }
  }

  return Error;
}

retCode dumpMethod(objectPo r,void *c)
{
  mtdPo mtd = (mtdPo)r;
  ioPo io = (ioPo)c;
  outMsg(io,"method %U: %U\n",mtd->name,methodSignature(mtd));

  assemInsPo ins = mtd->first;
  while(ins!=Null){
    tryRet(dumpIns(io,mtd,ins));
    ins = ins->next;
  }

  listPo lcl = mtd->locals;
  while(lcl!=nilList){
    localVarPo local = (localVarPo)head(lcl);

    tryRet(showLocal(io,mtd,local));

    lcl = tail(lcl);
  }

  return outMsg(io,"free: %U\n",freeSignature(mtd));
}

void dumpPkgCode(pkgPo pkg)
{
  outMsg(logFile,"Package %U:\n",pkg->name);
  processList(pkg->methods,dumpMethod,logFile);
  flushOut();
}


