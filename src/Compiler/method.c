/*
 * Handle method blocks
 */
#include "compiler.h"
#include "methodP.h"

#include <ooio.h>
#include <formioP.h>

static poolPo tryBlockPool;
static poolPo mtdPool;
static poolPo literalPool;

void initMethod()
{
  mtdPool = newPool(sizeof(Method),32);
  tryBlockPool = newPool(sizeof(TryBlock),64);
  literalPool = newPool(sizeof(LiteralRecord),256);
}

mtdCxtPo newMethod(char *name)
{
  mtdCxtPo mtd = (mtdCxtPo)allocPool(mtdPool);
  mtd->defName = uniIntern(name);
  mtd->literals = emptyList;
  mtd->tryBlocks = Null;
  mtd->scanBlocks = Null;
  mtd->code = newAssem(name);
  mtd->generated = Null;
  return mtd;
}

assemPo methodCode(mtdCxtPo mtd)
{
  return mtd->code;
}

cafeFun genMethodCode(mtdCxtPo mtd,lPo entryPoint)
{
  return mtd->generated = generateCode(mtd->code,entryPoint);
}

static logical isStringLiteral(void *d,void *cl)
{
  literalPo lit = (literalPo)d;
  char *str = (char*)cl;
  return lit->type==stringLiteral && uniCmp(lit->lit.str,str)==0;
}

lPo defineLiteralString(mtdCxtPo mtd,char *str)
{
  assemPo code = methodCode(mtd);
  literalPo lit = findInList(mtd->literals,isStringLiteral,str);

  if(lit==Null){
    lit = allocPool(literalPool);

    lit->type = stringLiteral;
    lit->lit.str = str;

    char *curr = currSegment(code);
    setSegment(code,dataSegment);
    AAlignTo(code,POINTER_SIZE);
    lit->lbl = currLbl(code,genSym(".S"));
    AConstS(code,str);
    setSegment(code,curr);

    mtd->literals = cons(lit,mtd->literals);
  }

  return lit->lbl;
}

static logical isOtherLiteral(void *d,void *cl)
{
  literalPo lit = (literalPo)d;
  return lit->type==otherLiteral && lit->lit.other.add==cl;
}

lPo defineLiteralOther(mtdCxtPo mtd,void *data, long size)
{
  literalPo lit = findInList(mtd->literals,isOtherLiteral,data);
  if(lit==Null){
    lit = allocPool(literalPool);

    lit->type = otherLiteral;
    lit->lit.other.add = data;
    lit->lit.other.size = size;

    assemPo code = methodCode(mtd);
    char *curr = currSegment(code);
    setSegment(code,dataSegment);
    AAlignTo(code,POINTER_SIZE);
    lit->lbl = currLbl(code,genSym(".O"));
    
    assert(False);
    setSegment(code,curr);

    mtd->literals = cons(lit,mtd->literals);
  }
  return lit->lbl;
}

static retCode dumpLiteral(void *r,void *cl)
{
  literalPo lit = (literalPo)r;
  ioPo io = (ioPo)cl;

  outMsg(io,"%B: ",lit->lbl);
  switch(lit->type){
  case integerLiteral:
    return outMsg(io,"%ld\n",lit->lit.i);
  case floatLiteral:
    return outMsg(io,"%f\n",lit->lit.d);
  case stringLiteral:
    return outMsg(io,"\"%#U\"\n",lit->lit.str);
  case otherLiteral:
    return outMsg(io,"0x%x(%x)\n",lit->lit.other.add,lit->lit.other.size);
  default:
    return Error;
  }
}

void addCatchBlock(mtdCxtPo mtd,lPo start,lPo end,lPo recover)
{
  tryPo catch = (tryPo)allocPool(tryBlockPool);
  catch->from = start;
  catch->to = end;
  catch->catchCode = recover;
  catch->next = mtd->tryBlocks;
  mtd->tryBlocks = catch;
}

tryPo methodCatchBlocks(mtdCxtPo mtd)
{
  return mtd->tryBlocks;
}

lPo tryBlockStart(tryPo try){
  return try->from;
}

lPo tryBlockEnd(tryPo try){
  return try->to;
}

lPo tryBlockRecover(tryPo try){
  return try->catchCode;
}

tryPo tryBlockNext(tryPo try){
  return try->next;
}

static retCode dumpRef(void *data,void *cl)
{
  varInfoPo var = (varInfoPo)data;
  ioPo io = (ioPo)cl;
  return outMsg(io,"  %U %s[%s]@%d\n",var->name,sourceName(var->where),
		kindName(var->kind),
		var->l.off);
}

static retCode dumpGcBlock(gcScanPo block,ioPo io)
{
  outMsg(io,"site: %B -> %B\n",block->callSite,block->scanCode);
  return processList(block->references,dumpRef,io);
}

static retCode dumpCallSites(mtdCxtPo mtd,ioPo io)
{
  gcScanPo block = mtd->scanBlocks;
  retCode ret = Ok;
  while(ret==Ok && block!=Null){
    ret = dumpGcBlock(block,io);
    block = block->next;
  }
  return ret;
}

static retCode dumpMethod(void *v,void *c)
{
  mtdCxtPo mtd = (mtdCxtPo)v;
  ioPo io = (ioPo)c;

  outMsg(io,"Definition: %U\n",mtd->defName);
  //  ProcessTable(dumpSegment,mtd->segments,c);
  outMsg(io,"Literals:\n");
  processList(mtd->literals,dumpLiteral,c);
  outMsg(io,"Call sites:\n");
  dumpCallSites(mtd,io);
  return Ok;
}

void dM(mtdCxtPo mtd)
{
  dumpMethod(mtd,logFile);
  flushOut();
}

