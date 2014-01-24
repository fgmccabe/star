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

static retCode showConstant(ioPo f,mtdPo mtd,int32 ix)
{
  constPo con = poolConstant(mtd,ix);
  if(con!=Null)
    return con->show(f,con);
  else
    return outMsg(f," <bad constant %d>\n",ix);
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

#define instruction(Op,A1,AOp,Cmt) \
    case Op:\
      return disp_##A1(f,mtd,ins,#AOp);

#include "instructions.h"
  case DefineLbl:
    return outMsg(f,"%B:\n",ins->lbl);

  case DefineFrame:
    tryRet(outMsg(f,"frame "));
    return showConstant(f,mtd,ins->i);

  default:
    return Error;
  }
}

static retCode showLocal(ioPo o,mtdPo mtd,localVarPo lcl)
{
  constPo c = poolConstant(mtd,lcl->name);
  if(c!=Null){
    uniChar *name = c->value.txt;

    c = poolConstant(mtd,lcl->sig);

    if(c!=Null){
      uniChar *sig = c->value.txt;

      outMsg(o,"%U:%U [%d]",name,sig,lcl->off);

      if(lcl->from->pc!=Null && lcl->to->pc!=Null)
	return outMsg(o," (0x%x-0x%x)",lcl->from->pc->pc,lcl->to->pc->pc);
      else
	return Ok;
    }
  }

  return Error;
}

retCode dumpMethod(void *r,void *c)
{
  mtdPo mtd = (mtdPo)r;
  ioPo io = (ioPo)c;
  outMsg(io,"method %U:\n",mtd->name);

  assemInsPo ins = mtd->first;
  while(ins!=Null){
    tryRet(dumpIns(io,mtd,ins));
    ins = ins->next;
  }

  listPo lcl = mtd->locals;
  while(lcl!=emptyList){
    localVarPo local = (localVarPo)head(lcl);

    tryRet(showLocal(io,mtd,local));

    lcl = tail(lcl);
  }

  return Ok;
}

void dumpPkgCode(pkgPo pkg)
{
  outMsg(logFile,"%U:\n",pkg->name);
  processList(pkg->methods,dumpMethod,logFile);
  flushOut();
}


