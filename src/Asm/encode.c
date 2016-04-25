/*
 * encode a bunch of methods into object file format
 */
#include "config.h"
#include <ooio.h>
#include <math.h>
#include "cafeOptions.h"
#include "encoding.h"
#include "assemP.h"
#include <assert.h>

static retCode encodeIns(ioPo out,assemInsPo ins);

static retCode encodeInt(ioPo out,int64 ix,int tag);
static retCode encodeMethod(ioPo out,mtdPo mtd);
static retCode encodeStr(ioPo out,uniChar *str,uniChar tag);

static retCode encodeFrame(ioPo out,mtdPo mtd,assemInsPo ins);
static retCode encodeLocal(ioPo out,mtdPo mtd,localVarPo lcl);

static retCode encMtd(void *r,void *c)
{
  ioPo out = (ioPo)c;
  mtdPo mtd = (mtdPo)r;

  return encodeMethod(out,mtd);
}

retCode encodePkg(ioPo out,pkgPo pkg)
{
  tryRet(encodeInt(out,listCount(pkg->methods),trmDct));	/* pkg dictionary */

  return processList(pkg->methods,encMtd,out); /* process all the methods */
}

static retCode encodeConstant(ioPo out,constPo con)
{
  tryRet(encodeString(out,con->sig));
  return con->encode(out,con);
}

retCode encodeMethod(ioPo out,mtdPo mtd)
{
  assert(mtd->sig==0 && mtd->freeSig == 1);

  tryRet(encodeStr(out,mtd->name,trmTag)); /* signal tag w/name */

  tryRet(outByte(out,trmCde));		/* signal a code block */
  tryRet(encodeInteger(out,mtd->sig));        /* Constant id for the type signature */
  tryRet(encodeInteger(out,mtd->freeSig));    /* Which constant is the free signature? */
  tryRet(encodeInteger(out,poolCount(mtd)));
  tryRet(encodeInteger(out,frameCount(mtd))); /* Number of frame records */
  tryRet(encodeInteger(out,localCount(mtd))); /* Number of local var records */
  tryRet(encodeInteger(out,codeSize(mtd)));

  for(listPo con = mtd->constants;con!=emptyList;con=tail(con)){
    tryRet(encodeConstant(out,(constPo)head(con)));
  }

  for(assemInsPo pc = mtd->first;pc!=Null;pc=pc->next){
    tryRet(encodeIns(out,pc));
  }

  for(assemInsPo pc = mtd->first;pc!=Null;pc=pc->next){
    tryRet(encodeFrame(out,mtd,pc));
  }

  for(listPo lcl = mtd->locals; lcl!=emptyList; lcl=tail(lcl)){
    tryRet(encodeLocal(out,mtd,(localVarPo)head(lcl)));
  }
  return Ok;
}

retCode encodeFrame(ioPo out,mtdPo mtd,assemInsPo ins)
{
  if(ins->op==DefineFrame){
    tryRet(encodeInteger(out,ins->i));
    tryRet(encodeInteger(out,ins->pc/sizeof(uint16)));
  }
  return Ok;
}

retCode encodeLocal(ioPo out,mtdPo mtd,localVarPo lcl)
{
  tryRet(encodeInteger(out,lcl->name));
  tryRet(encodeInteger(out,lcl->sig));
  tryRet(encodeInteger(out,lcl->off));
  tryRet(encodeInteger(out,lcl->from->pc->pc/sizeof(uint16)));
  return encodeInteger(out,lcl->to->pc->pc/sizeof(uint16));
}

static retCode enc_tos(ioPo out,assemInsPo ins)
{
  return Ok;
}

static retCode enc_nOp(ioPo out,assemInsPo ins)
{
  return Ok;
}

static retCode enc_i32(ioPo out,assemInsPo ins)
{
  return encodeInteger(out,ins->i);
}

static retCode enc_arg(ioPo out,assemInsPo ins)
{
  return encodeInteger(out,ins->i);
}

static retCode enc_lcl(ioPo out,assemInsPo ins)
{
  return encodeInteger(out,ins->i);
}

static retCode enc_env(ioPo out,assemInsPo ins)
{
  return encodeInteger(out,ins->i);
}

static retCode enc_off(ioPo out,assemInsPo ins)
{
  return encodeInteger(out,ins->i);
}

static retCode enc_lit(ioPo out,assemInsPo ins)
{
  return encodeInteger(out,ins->i);
}

static retCode enc_Es(ioPo out,assemInsPo ins)
{
  return encodeInteger(out,ins->i);
}

retCode encodeIns(ioPo out,assemInsPo ins)
{
  switch(ins->op){
#undef instruction
#define instruction(Op,A1,A2,Cmt)		\
    case Op:					\
      tryRet(outByte(out,Op));			\
      return enc_##A1(out,ins);
#include "instructions.h"
  case DefineLbl:
  case DefineFrame:
    return Ok;
  default:
    return Error;
  }
}

retCode encodeInt(ioPo out,int64 ix,int tag)
{
  int len=0,i;
  byte bytes[16];		/* don't really need 16 */
  unsigned int64 v = ix;

  if(v==0)			 /* special case for 0 to allow large numbers */
    bytes[len++]=0;
  else
    while(v>0){
      bytes[len++]=v&0xff;
      v >>= 8;
    }

  if(ix>0 && (bytes[len-1]&0x80))	/* correct issues to do with signs */
    bytes[len++]=0;

  tryRet(outByte(out,tag|len));
  for(i=len;i>0;i--)
    tryRet(outByte(out,bytes[i-1]));

  return Ok;
}

retCode encodeInteger(ioPo out,int64 ix)
{
  return encodeInt(out,ix,trmInt);
}

retCode encodeStr(ioPo out,uniChar *str,uniChar tag)
{
  int64 len = uniStrLen(str);
  byte buff[3*len+1];
  int64 ulen = uni_utf8(str,len,buff,3*len+1);
  long actual;

  tryRet(encodeInt(out,ulen,tag));	/* The UTF8 length of the string */

  return outBytes(out,buff,ulen,&actual);
}

retCode encodeString(ioPo out,uniChar *str)
{
  return encodeStr(out,str,trmStr);
}

retCode encodeRef(ioPo out,uniChar *str)
{
  return encodeStr(out,str,trmRef);
}

retCode encodeEscapeRef(ioPo out,uniChar *str)
{
  return encodeStr(out,str,trmEsc);
}

retCode encodeFloat(ioPo out,double dx)
{
  if(dx<0.0){
    tryRet(outByte(out,trmFlt|1));
    dx = -dx;
  }
  else
    tryRet(outByte(out,trmFlt));

  int exp;
  
  double f = frexp(dx,&exp);			/* get the exponent */
  tryRet(encodeInteger(out,exp));

  int len;
  
  for(len=0;f!=0.0;len++){
    double ip;
    f = modf(f*256,&ip);

    tryRet(outByte(out,(int)ip));
  }

  return Ok;
}
