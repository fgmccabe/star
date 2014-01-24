#include "config.h"
#include "ooio.h"
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>

#include "cafeOptions.h"
#include "utils.h"
#include "engine.h"
#include "decode.h"
#include "escapes.h"
#include "opcodes.h"
#include "codeP.h"

/*
 * This implements a binary decoder. Can be used to load executable code.
 */

static retCode dec_nOp(ioPo in,OpCode op,insPo *code);
static retCode dec_i32(ioPo in,OpCode op,insPo *code);
static retCode dec_arg(ioPo in,OpCode op,insPo *code);
static retCode dec_lcl(ioPo in,OpCode op,insPo *code);
static retCode dec_env(ioPo in,OpCode op,insPo *code);
static retCode dec_off(ioPo in,OpCode op,insPo *code);
static retCode dec_lit(ioPo in,OpCode op,insPo *code);
static retCode dec_Es(ioPo in,OpCode op,insPo *code);

static retCode decFlt(ioPo in,double *res,uniChar tag);
static retCode decString(ioPo in,uniChar **data,uniChar tag);
static retCode decodeMtd(ioPo in,methodPo *mtd,hashPo dict);

static retCode decInt(ioPo in,integer *ii,uniChar tag);
static retCode decRef(ioPo in,void **data,hashPo dict,uniChar tag);
static retCode decEscapeRef(ioPo in,void **data,hashPo dict,uniChar tag);

static retCode decodePkgEntry(ioPo in,hashPo dict);

hashPo decodePkg(ioPo in)
{
  hashPo dict = NewHash(256,(hashFun)uniHash,(compFun)uniCmp,Null);
  uniChar ch = inCh(in);
  integer count;

  assert(isTag(ch,trmDct));


  retCode ret = decInt(in,&count,ch);

  for(integer ix=0;ret==Ok && ix<count;ix++)
    ret = decodePkgEntry(in,dict);

  if(ret==Ok)
    return dict;
  else{
    DelHash(dict);
    return Null;
  }
}

retCode decodePkgEntry(ioPo in,hashPo dict)
{
  uniChar ch = inCh(in);

  if(!isTag(ch,trmTag))
    return Error;
  else{
    uniChar *name;
    methodPo mtd;
    tryRet(decString(in,&name,ch));

    if(!isTag(inCh(in),trmCde))
      return Error;

    tryRet(decodeMtd(in,&mtd,dict));
    Install(name,mtd,dict);
    return Ok;
  }
}

static retCode decodeTerm(ioPo in,void *data,hashPo dict)
{
  uniChar ch;
  retCode res = inChar(in,&ch);

  if(res==Ok){
    switch(ch&trmMask){
    case trmInt:{
      integer ii;
      retCode ret = decInt(in,&ii,ch);

      *((int32*)data) = (int32)ii;
      return ret;
    }
    case trmFlt:
      return decFlt(in,(double*)data,ch);
    case trmStr:
      return decString(in,(uniChar**)data,ch);
    case trmCde:
      return decodeMtd(in,(methodPo*)data,dict);
    case trmRef:
      return decRef(in,data,dict,ch);
    case trmEsc:
      return decEscapeRef(in,data,dict,ch);
    default:
      return Error;
    }
  }
  return res;
}

static retCode decInt(ioPo in,integer *ii,uniChar tag)
{
  integer len = tag&0xf;
  integer result = 0;
  int i;
  retCode ret = Ok;
  uniChar ch;

  if(len==0)			/* recursive length */
    ret = decInt(in,&len,inCh(in));

  for(i=0;i<len;i++){
    ch = inCh(in);
    
    if(i==0)
      result = (signed char)(ch&0xff);
    else
      result = (result<<8)|ch;
  }

  if(ii!=NULL)
    *ii = result;
  return ret;
}

static retCode decFlt(ioPo in,double *res,uniChar tag)
{
  byte ch;
  retCode ret = inByte(in,&ch);
  unsigned short len = ch&0x7;
  int sign = (tag&0xf)==0?1:-1;
  byte buff[16];
  int i;

  integer exp;

  if((ret=decInt(in,&exp,inCh(in)))!=Ok)
    return ret;

  for(i=0;i<len;i++)
    buff[i]=inCh(in);

  double f = 0.0;
  while(len--)
    f = (f + buff[len])/256;

  f = ldexp(f,exp);
  *res = sign*f;
  return Ok;
}

retCode decInteger(ioPo in,integer *ii)
{
  uniChar ch;
  retCode ret = inChar(in,&ch);
  if(ret==Ok)
    return decInt(in,ii,ch);
  else
    return ret;
}

retCode decStr(ioPo in,uniChar *uData,integer buffLen,integer *actual)
{
  integer len = 0;
  uniChar ch;
  retCode res = inChar(in,&ch);

  if(res==Ok && ((ch&trmMask)!=trmStr))
    res = Error;

  if(res==Ok && (res=decInt(in,&len,ch))!=Ok)
    return res;
  else{
    byte bBuff[2048];
    byte *bData = (len<NumberOf(bBuff)?bBuff:(byte*)(malloc(len*sizeof(byte)))); 
    long blen;

    *actual = len;
      
    res=inBytes(in,bData,len,&blen); /* read in a block of bytes */
      
    if(len>=2 && bData[0]==uniBOMhi && bData[1]==uniBOMlo){
      integer j=0;
      for(integer i=2;i<len && j<buffLen;i+=2,j++)
	uData[j]=((bData[i]&0xff)<<8)|(bData[i+1]&0xff);

      if(j>=buffLen)
	res = Error;
    }
    else if(len>=2 && bData[1]==uniBOMhi && bData[0]==uniBOMlo){
      integer j = 0;
      for(integer i=2;i<len && j<buffLen;i+=2,j++)
	uData[j]=((bData[i+1]&0xff)<<8)|(bData[i]&0xff);

      if(j>=buffLen)
	res = Error;
    }
    else{
      integer i;
      for(i=0;i<len && i<buffLen;i++)
	uData[i] = bData[i]&0xff;
      if(i>=buffLen)
	res = Error;
    }

    if(bData!=bBuff)
      free(bData);
  }
  return res;
}

retCode decString(ioPo in,uniChar **uData,uniChar tag)
{
  integer len,actLen;
  retCode res = Ok;

  if(res!=Ok || (res=decInt(in,&len,tag))!=Ok)
    return res;
  else{
    byte bBuff[2048];
    byte *bData = (len<NumberOf(bBuff)?
		   bBuff:(byte*)(malloc((len+1)*sizeof(byte)))); 
    uniChar uBuff[2048];
    uniChar *buffer = len<NumberOf(uBuff)?
      uBuff:(uniChar*)malloc((len+1)*sizeof(uniChar));
    long blen = 0;

    res=inBytes(in,bData,len,&blen); /* read in a block of bytes */
      
    if(len>=2 && bData[0]==uniBOMhi && bData[1]==uniBOMlo){
      integer j = 0;
      for(integer i=2;i<len && j<blen;i+=2,j++)
	buffer[j]=((bData[i]&0xff)<<8)|(bData[i+1]&0xff);
      actLen = j;
    }
    else if(len>=2 && bData[1]==uniBOMhi && bData[0]==uniBOMlo){
      integer j = 0;
      for(integer i=2;i<len && j<blen;i+=2,j++)
	buffer[j]=((bData[i+1]&0xff)<<8)|(bData[i]&0xff);
      actLen = j;
    }
    else{
      integer i;
      for(i=0;i<len && i<blen;i++)
	buffer[i] = bData[i]&0xff;
      actLen = i;
    }

    uniChar *text = (uniChar*)malloc(sizeof(uniChar)*(actLen+1));
    uniNCpy(text,actLen+1,buffer,actLen);

    if(bData!=bBuff)
      free(bData);
    if(buffer!=uBuff)
      free(buffer);
    *uData = text;			/* return the result */
  }
  return res;
}

retCode decRef(ioPo in,void **data,hashPo dict,uniChar tag)
{
  uniChar *name;

  tryRet(decString(in,&name,tag));

  *data = hashGet(dict,name);

  if(*data==Null)
    *data = findEscape(name);
  return Ok;
}

retCode decEscapeRef(ioPo in,void **data,hashPo dict,uniChar tag)
{
  uniChar *name;

  tryRet(decString(in,&name,tag));

  *data = findEscape(name);
  return (*data!=Null?Ok:Error);
}

static logical earlyPc(void *l,void *r)
{
  localPtr lhs = (localPtr)l;
  localPtr rhs = (localPtr)r;

  return lhs->from<=rhs->from;
}

static void swapLcl(void *l,void *r)
{
  localPtr lhs = (localPtr)l;
  localPtr rhs = (localPtr)r;

  LocalRec s = *lhs;
  *lhs = *rhs;
  *rhs = s;
}

static void *getLcl(void *data,int64 ix)
{
  localPtr base = (localPtr)data;
  return base+ix;
}

/*
 * A code block looks like:
 *   <codesize>
 *   <constantcount>
 *   <framecount>
 *   <localcount>
 *     <constant>*
 *   (<opCode><Operands>*)*
 *   (<sig><pc>)*  frames
 *   (<id><sig><lcl><from><to>)* locals
 *
 * The first constant must be the signature of the function.
 * The second constant must be the signature of free variables
 * The third constant is usually the name of the function. Used for debugging purposes only.
 *
 */
retCode decodeMtd(ioPo in,methodPo *tgt,hashPo dict)
{
  int64 codeSize,poolCount,frameCount,localCount;

  tryRet(decInteger(in,&poolCount));
  tryRet(decInteger(in,&frameCount));
  tryRet(decInteger(in,&localCount));
  tryRet(decInteger(in,&codeSize));
  
  methodPo mtd = *tgt = (methodPo)malloc(sizeof(MethodRec)+poolCount*sizeof(ConstantRec));

  mtd->poolCount = poolCount;
  mtd->codeSize = codeSize;
  mtd->frameCount = frameCount;
  mtd->localCount = localCount;

  constantPo constantPool = &mtd->pool[0];

  for(integer ix=0;ix<poolCount;ix++){
    constantPo con = &constantPool[ix];
    tryRet(decString(in,&con->sig,inCh(in)));
    tryRet(decodeTerm(in,&con->data,dict));
  }

  insPo code = mtd->code = (insPo)malloc(sizeof(uint16)*codeSize);
  retCode ret = Ok;
  insPo codeLimit = code+codeSize;

  while(ret==Ok && code<codeLimit){
    byte nxt;
    ret = inByte(in,&nxt);

    if(ret==Ok){
      OpCode op = (OpCode)nxt;
      switch(op){
#undef instruction
#define instruction(Op,A1,A2,Cmt)				\
	case Op: ret = dec_##A1(in,Op,&code);			\
	  continue;
#include "instructions.h"
#undef instruction
      case DefineLbl:
      case DefineFrame:
      case illegalOp:
	ret = Error;
      }
    }
  }

  framePtr frames = mtd->frames = (framePtr)malloc(sizeof(FrameRec)*frameCount);
  localPtr locals = mtd->locals = (localPtr)malloc(sizeof(LocalRec)*localCount);

  for(int32 ix=0;ix<frameCount;ix++){
    integer sig,pc;
    tryRet(decInteger(in,&sig));
    tryRet(decInteger(in,&pc));
    frames[ix].sig = sig;
    frames[ix].pc = pc;
  }

  for(int32 ix=0;ix<localCount;ix++){
    integer sig,name,off,from,to;
    tryRet(decInteger(in,&name));
    tryRet(decInteger(in,&sig));
    tryRet(decInteger(in,&off));
    tryRet(decInteger(in,&from));
    tryRet(decInteger(in,&to));
    locals[ix].name = name;
    locals[ix].sig = sig;
    locals[ix].off = off;
    locals[ix].from = from;
    locals[ix].to = to;
  }

  sort(locals,localCount,earlyPc,swapLcl,getLcl);

  tryRet(functionArity(mtdSignature(mtd),&mtd->arity));
  tryRet(tupleArity(mtdFreeSignature(mtd),&mtd->freeCount));

  mtd->jit = NULL;
  mtd->jitSize = -1;

  return ret;
}

retCode dec_nOp(ioPo in,OpCode op,insPo *code)
{
  *(*code)++ = op;
  return Ok;
}

retCode dec_i32(ioPo in,OpCode op,insPo *code)
{
  *(*code)++ = op;

  integer ix;

  retCode ret = decInteger(in,&ix);

  *(*code)++ = (ix>>16)&0xffff;		/* big endian order */
  *(*code)++ = ix&0xffff;

  return ret;
}

retCode dec_arg(ioPo in,OpCode op,insPo *code)
{
  return dec_i32(in,op,code);
}

retCode dec_lcl(ioPo in,OpCode op,insPo *code)
{
  return dec_i32(in,op,code);
}

retCode dec_env(ioPo in,OpCode op,insPo *code)
{
  return dec_i32(in,op,code);
}

retCode dec_off(ioPo in,OpCode op,insPo *code)
{
  return dec_i32(in,op,code);
}

retCode dec_lit(ioPo in,OpCode op,insPo *code)
{
  return dec_i32(in,op,code);
}

retCode dec_Es(ioPo in,OpCode op,insPo *code)
{
  return dec_i32(in,op,code);
}

