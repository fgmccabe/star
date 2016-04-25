// Incremental instruction debugger

#include "config.h"
#include "engine.h"
#include "signature.h"
#include "escapes.h"
#include <stdlib.h>

static retCode showConstant(ioPo out,closurePo cl,int off);

static void showRegisters(int32 pcCount,processPo p,closurePo env,insPo pc,framePo fp,ptrPo sp);

void debug_stop(int32 pcCount,processPo p,closurePo env,insPo pc,framePo fp,ptrPo sp)
{
  int ch;
  static uniChar line[256] = {'n',0};

  static processPo focus = NULL; /* non-null implies only interested in this */
  static int32 traceCount = 0;

  if(focus==NULL || focus==p){
    disass(pcCount,p,env,pc,fp,sp);
    if(!interactive || traceCount>0){
      if(traceCount==0)
      	outMsg(logFile,"\n");
      else{
      	traceCount--;
      	if(traceCount>0)
      	  outMsg(logFile,"\n");
      }
      flushFile(logFile);
    }

    while(interactive && traceCount==0){
      uniChar *ln = line;
      outMsg(logFile," => ");
      flushFile(logFile);

      //      reset_stdin();

      if((ch=inCh(stdIn))!='\n' && ch!=uniEOF){
      	do{
      	  *ln++=ch;
      	  ch = inCh(stdIn);
      	} while(ch!='\n' && ch!=uniEOF);
      	*ln++='\0';
      }

      //      setup_stdin();
    
      switch(line[0]){
      case ' ':
      case 'n':
      case '\n':
      	break;
      case 'f':
      	focus = p;
      	uniLit(line,NumberOf(line),"n\n");
      	break;
      case 'u':
      	focus = NULL;
      	uniLit(line,NumberOf(line),"n\n");
      	break;
      case 'q':
      	outMsg(logFile,"terminating session");
      	exit(0);

      case 't':
      	interactive = False;
      	break;
      case uniEOF:
      case 'c':
      	tracing=False;
      	break;
      case 'r':			/* dump the registers */
      	showRegisters(pcCount,p,env,pc,fp,sp);
      	continue;
      case 'l':{		/* dump a local variable */
      	logMsg(logFile,"not implemented\n");
      	continue;
      }
      case 'e':{		/* dump an environment variable */
      	logMsg(logFile,"not implemented\n");
      	continue;
      }
      case 'P':{		/* Display all processes */
      	logMsg(logFile,"not implemented\n");
      	continue;
      }

      case 's':			/* Show a stack trace of this process */
      	logMsg(logFile,"not implemented\n");
      	continue;

      case '0': case '1': case '2': case '3': case '4': case '5': 
      case '6': case '7': case '8': case '9': {
      	traceCount = parseInteger(line,uniStrLen(line));
      	continue;
      }
      
      case 'i':{
      	integer off=parseInteger(line+1,uniStrLen(line+1));
      	integer i;
      	insPo pc0 = pc;
      	
      	for(i=0;i<off;i++){
      	  pc0 = disass(pcCount+i,p,env,pc0,fp,sp);
      	  outChar(logFile,'\n');
      	}
      	uniLit(line,NumberOf(line),"n\n");
      	continue;
      }
        
      default:
      	outMsg(logFile,"'n' = step, 'c' = continue, 't' = trace mode, 'q' = stop\n");
      	outMsg(logFile,"'<n>' = step <n>\n");
      	outMsg(logFile,"'r' = registers, 'l <n>' = local, 'e <n>' = env var\n");
      	outMsg(logFile,"'i'<n> = list n instructions, 's' = stack trace\n");
      	outMsg(logFile,"'f' = focus on this process, 'u' = unfocus \n");
      	continue;
      }
      return;
    }
  }
}

#define collectI32(pc) (collI32(pc))
#define collI32(pc) hi32 = (uint32)(*pc++), lo32 = *pc++, ((hi32<<16)|lo32)

static void showEscape(closurePo cl,int32 escNo)
{
  constantPo escCon = &codeLiterals(cl)[escNo];
  escapePo esc = (escapePo)escCon->data;

  outMsg(logFile," (%U)",esc->name);
}

insPo disass(int32 pcCount,processPo p,closurePo env,insPo pc,framePo fp,ptrPo sp)
{
  int32 hi32,lo32;

  outMsg(logFile,"0x%x [%d]",pc,pcCount);

  switch(*pc++){
#undef instruction

#define show_nOp
#define show_tos
#define show_i32 outMsg(logFile," #%d",collectI32(pc))
#define show_arg outMsg(logFile," a[%d]",collectI32(pc))
#define show_lcl outMsg(logFile," l[%d]",collectI32(pc))
#define show_env outMsg(logFile," e[%d]",collectI32(pc))
#define show_off outMsg(logFile," 0x%x",(collI32(pc)+pc))
#define show_Es showEscape(env,collectI32(pc))
#define show_lit showConstant(logFile,env,collectI32(pc))

#define instruction(AOp,A1,Op,Cmt)		\
    case AOp:					\
      outMsg(logFile," %s",#Op);		\
      show_##A1;				\
	return pc;

#include "instructions.h"

  default:
    return pc;
  }
}

static int showBySig(ioPo out,uniChar *sig,int32 pos,void *data);

static retCode showConstant(ioPo out,closurePo cl,int off)
{
  methodPo mtd = clMethod(cl);

  void *data = mtd->pool[off].data;
  uniChar *sig = mtd->pool[off].sig;
  showBySig(out,sig,0,data);
  return Ok;
}

void showRegisters(int32 pcCount,processPo p,closurePo env,insPo pc,framePo fp,ptrPo sp)
{
  outMsg(logFile,"p: 0x%x, cl: 0x%x, pc: 0x%x, fp: 0x%x, sp: 0x%x\n",
	 p,env,pc,fp,sp);

  methodPo mtd = clMethod(env);
  int32 pcOffset = pc-mtd->code;

  localPtr locals = mtd->locals;

  for(int32 ix=0;ix<mtd->localCount;ix++){
    if(locals[ix].from<=pcOffset && locals[ix].to>pcOffset){
      int32 off = locals[ix].off;
      ptrPo var = localVar(fp,off);
      uniChar *vrName = mtd->pool[locals[ix].name].data;
      uniChar *sig = mtd->pool[locals[ix].sig].data;

      outMsg(logFile,"%U [%d]:%U ",vrName,off,sig);
      showBySig(logFile,sig,0,var);
      outMsg(logFile,"\n");
    }
  }
}

int showBySig(ioPo out,uniChar *sig,int32 pos,void *data)
{
  int32 end = uniStrLen(sig);

  showSignature(out,sig,&pos,end);
  return pos;
}

