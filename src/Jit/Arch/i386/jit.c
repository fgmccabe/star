/*
 * Assemble the virtual instructions using the JIT
 */

#include "config.h"
#include "assemP.h"
#include "heapP.h"
#include "code.h"
#include "utils.h"
#include "jitP.h"
#include "cafeOptions.h"

#include <pool.h>
#include <hash.h>
#include <iostr.h>

#include <sys/mman.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

extern logical debugAssem;

static int intRegs[] = { _EAX, _EDI, _ESI, _EDX, _ECX, _EBP, _R12, _R13};
static int fpRegs[] = { JIT_FPRET, JIT_FPR0, JIT_FPR1, JIT_FPR2, JIT_FPR3 };
static int tmpRegs[] = { _R10, _R11, _R9, _R8 };

typedef enum {
  codeRef, moveRef, constRef, callRef
} fixupType;

static void useLbl(insPo lbl,fixupType type, jit_insn *ref,asmCxtPo cxt);
static void assemIns(insPo ins,asmCxtPo cxt);

static void outOfMemError();

logical is16bit(integer ix);
/*
 * A normal frame looks like:
 *
 * |  Prev FP address       |<-\
 * +------------------------+  |
 * |                        |  |
 * |  Locals of prior frame |  |
 * |                        |  |
 * +------------------------+  |
 * |                        |  |   <---- nth argument
 * |  Call Args             |  |
 * |                        |  |
 * +------------------------+  |
 * |  Saved ENV pointer     |  |   <---- also 1st argument
 * +------------------------+  |
 * |  Return address        |  |
 * +------------------------+  |
 * |  Saved FP address      | -/  <---- FP reg
 * +------------------------+
 * |                        |
 * |  Locals of curr frame  |
 * |                        |
 * +----- top of stack -----+
 * |                        |
 *
 */

#define ENV_REG intRegs[ENV]
#define T_REG intRegs[TRM]

#define tryJit(Op) {				\
    assert((void*)cxt->state.x.pc-cxt->buffer<cxt->size);	\
    if(cxt->mode==counting){			\
      jit_insn *old = cxt->state.x.pc;		\
      assert(old==cxt->buffer);			\
      Op;					\
      cxt->pc+=jit_get_label()-old;		\
      cxt->state.x.pc = old;			\
    }						\
    else{					\
      Op;					\
      cxt->pc+=jit_get_label()-cxt->state.x.pc;	\
    }						\
    assert((void*)cxt->state.x.pc-cxt->buffer<=cxt->size);	\
  }

#ifdef TRACEASSM
#define debug(Op) if(debugAssem && cxt->mode==generating){	\
    outMsg(logFile,"%x: ",cxt->state.x.pc);		 \
    Op;							 \
}
#else
#define debug(Op)
#endif


static void align(asmCxtPo cxt,int size)
{
  if(cxt->mode==counting)
    cxt->pc = ALIGN(cxt->pc,size);
  else{
    jit_insn *old = cxt->state.x.pc;
    cxt->state.x.pc = (jit_insn *)ALIGN(cxt->state.x.pc,size);
    cxt->pc += cxt->state.x.pc-old;
  }
}

#define retrn() {				\
    MOVQrr(_EBP,_ESP);				\
    POPQr(_EBP);				\
    MOVQmr(ENV_ARG_OFFSET,_EBP,0,0,ENV_REG);	\
    RET_();					\
  }
#define retrnC() {LEAVE_(); POPQr(_R13); POPQr(_R12); RET_();}

static uinteger insHash(void *p)
{
  return (uinteger)p;
}

static int insCmp(void *l,void *r)
{
  uinteger LL = (uinteger)l;
  uinteger RR = (uinteger)r;

  if(LL<RR)
    return -1;
  else if(LL>RR)
    return 1;
  else
    return 0;
}

asmCxtPo newAssemCxt(long size,methodPo mtd)
{
#define _jit          (cxt->state)
  asmCxtPo cxt = (asmCxtPo)malloc(sizeof(AssembleContext));
  jit_state state = jit_init();

  cxt->pc = 0;
  cxt->state = state;
  cxt->buffer = mmap(NULL, size, PROT_EXEC | PROT_READ | PROT_WRITE,
		     MAP_PRIVATE | MAP_ANON, -1, 0);
  cxt->size = size;
  cxt->mode = counting;

  cxt->state.x.pc = cxt->buffer;

  cxt->lbls = NewHash(1024,insHash,insCmp,Null);

  cxt->mtd = mtd;

  return cxt;
#undef _jit
}

void restartAssem(asmCxtPo cxt,long newSize)
{
#define _jit          (cxt->state)
  munmap(cxt->buffer,cxt->size);

  jit_state state = jit_init();

  cxt->buffer = mmap(NULL, newSize, PROT_EXEC | PROT_READ | PROT_WRITE,
		     MAP_PRIVATE | MAP_ANON, -1, 0);
  cxt->size = newSize;
  cxt->pc = 0;
  cxt->mode = generating;

  cxt->state = state;
  cxt->state.x.pc = cxt->buffer;
#undef _jit
}

void closeAssemCxt(asmCxtPo cxt)
{
  DelHash(cxt->lbls);
  free(cxt);
}

void assemble(asmCxtPo cxt)
{
  methodPo mtd = cxt->mtd;
  assert(mtd->code!=Null);
  long insCount = mtd->codeSize;

  insPo ins = mtd->code;
  for(long ix=0;ix<insCount;ix++)
    assemIns(ins,cxt);
}

cafeFun generateCode(methodPo mtd)
{
#define _jit          (cxt->state)
  asmCxtPo cxt = newAssemCxt(128,mtd);

  assemble(cxt);
  restartAssem(cxt,cxt->pc);
  assemble(cxt);
  
  jit_flush_code(cxt->buffer,jit_get_ip().ptr);

  cafeFun fun = (cafeFun)cxt->buffer;

  closeAssemCxt(cxt);
  return fun;
#undef _jit
}

static void assemIns(insPo ins,asmCxtPo cxt)
{
#define _jit (cxt->state)
  debug(dumpIns(logFile,ins));
  switch(ins->op){
  case Halt:
    tryJit({
	jit_prepare(1);
	jit_movi_i(JIT_R0,ins->o.oneInt.ix);
	jit_pusharg_i(JIT_R0);
	MOVQir((unsigned long)outOfMemError,JIT_R1);
	CALLsr(JIT_R1);			/* Not coming back from this */
      });
    return;

  case StartCall:{
    int depth = cxt->argDepth = ins->o.oneInt.ix;

    tryJit({
	if(depth!=0)
	  LEAQmr(depth,_EBP,0,0,_ESP);	/* adjust stack pointer */
      });
    return;
  }

  case StartCCall:{
    short depth = ins->o.threeIntIntInt.ix1;
    byte regCnt = ins->o.threeIntIntInt.ix2;
    byte fpCnt = ins->o.threeIntIntInt.ix3;

    tryJit({
	if(depth!=0)
	  LEAQmr(depth,_EBP,0,0,_ESP);	/* adjust stack pointer */
	jit_prepare(regCnt);
	jit_prepare_d(fpCnt);
      });
    return;
  }

  case Call:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	jit_pushr_p(reg);		/* The first argument */
	jit_ldr_p(JIT_REXTMP,reg);
	CALLsr(JIT_REXTMP);
      });
    return;
  }

    /**
     * The algorithm for tail recursion is a little complex, because we need to
     * get rid of the current locals as well as the current arguments.
     */
    /*
     * Just prior to a tail call, the stack should look like:
     *                <- FP (prev)
     * |  An        | 
     * |  .         |
     * |  A1        |
     * ++++++++++++++
     * | CLOS(prev) |
     * | RTN (prev) |
     * | FP (prev)  | <- FP
     * ++++++++++++++
     * |  Lk        |
     * |  .         |
     * |  L0        |
     * |  Bm        |
     * |  .         |
     * |  B1        | <- SP
     * ++++++++++++++
     * |            |
     * 
     * After the tail call, the stack should look like:
     *                <- FP (prev)
     * |  Bm        |
     * |  .         |
     * |  B1        |
     * ++++++++++++++
     * | CLOS (new) |
     * | RTN (prev) | <- SP
     * |            |
     *
     * where Ai are the outer call arguments, Li are the outer call locals
     * both sets of which are deleted.
     * The next instruction after the tail call will re-establish the
     * frame pointer for the new function being entered.
     */
  case Tail:{
    int reg = intRegs[ins->o.threeIntRegInt.reg];
    int depth = ins->o.threeIntRegInt.ix1;
    int outer = ins->o.threeIntRegInt.ix2;

    tryJit({
	int fpReg = intRegs[FP];
	int oldRtn = JIT_REXTMP;
        int dBase = tmpRegs[0];
        int sBase = tmpRegs[1];
	int tmp = tmpRegs[2];
	int four = tmpRegs[3];
        int cx = _CX;

	MOVQmr(RTN_OFFSET,FP,0,0,oldRtn); /* pick up old return address */
	LEAQmr(depth,_SP,0,0,sBase);	/* set src base to last arg */
	LEAQmr(outer+FIRST_ARG_OFFSET,fpReg,0,0,dBase); /* pick up dst base */
	MOVQmr(0,fpReg,0,0,fpReg);			/* pick up old FP */

	if(depth>0){
	  MOVQir(depth,cx);		/* Counter (must be multiple of 4) */
	  MOVQir(4,four);
	
	  jit_insn *loop = _jit.x.pc;
	  MOVLmr(0,sBase,0,0,tmp);	/* Copy one long word across */
	  MOVLrm(tmp,0,dBase,0,0);
	  SUBQrr(four,sBase);		/* Decrement pointers */
	  SUBQrr(four,dBase);
	  SUBLrr(four,cx);
	  TESTQrr(cx,cx);		/* Are there any more? */
	  JNZm(loop);			/* jump to loop */
	}

	// set up closure and return address
	MOVQrr(dBase,_ESP);
	PUSHQr(reg);			/* store the new closure */
	PUSHQr(oldRtn);			/* and the old return address */
	
	MOVQmr(0,reg,0,0,reg);		/* dereference the new closure */
	JMPsr(reg);			/* jump to code */
      });
    return;
  }

  case CallX:{
    int offset = ins->o.twoRegInteger.ix;
    int reg = intRegs[ins->o.twoRegInteger.reg];

    tryJit({
	jit_ldxi_p(JIT_REXTMP,reg,offset);
	PUSHQr(JIT_REXTMP);		/* The first argument */
	jit_ldr_p(JIT_REXTMP,JIT_REXTMP);
	CALLsr(JIT_REXTMP);
      });
    return;
  }

  case CallLbl:{
    insPo lbl = ins->o.oneLbl.lbl;

    tryJit({
	useLbl(lbl,moveRef,jit_movi_p(JIT_REXTMP,lbl),cxt);
	PUSHQr(JIT_REXTMP);		/* The first argument */
	jit_ldr_p(JIT_REXTMP,JIT_REXTMP);
	CALLsr(JIT_REXTMP);
      });
    return;
  }

    // Tail call to a fixed label
  case TailLbl:{
    insPo lbl = ins->o.threeIntLblInt.lbl;
    int depth = ins->o.threeIntLblInt.ix1;
    int outer = ins->o.threeIntLblInt.ix2;

    tryJit({
	int fpReg = intRegs[FP];
	int oldRtn = JIT_REXTMP;
        int dBase = tmpRegs[0];
        int sBase = tmpRegs[1];
	int tmp = tmpRegs[2];
	int four = tmpRegs[3];
        int cx = _CX;

	MOVQmr(RTN_OFFSET,FP,0,0,oldRtn); /* pick up old return address */
	LEAQmr(depth,_SP,0,0,sBase);	/* set src base to last arg */
	LEAQmr(outer+FIRST_ARG_OFFSET,fpReg,0,0,dBase); /* pick up dst base */
	MOVQmr(0,fpReg,0,0,fpReg);			/* pick up old FP */

	if(depth>0){
	  MOVQir(depth,cx);		/* Counter (must be multiple of 4) */
	  MOVQir(4,four);
	
	  jit_insn *loop = _jit.x.pc;
	  MOVLmr(0,sBase,0,0,tmp);	/* Copy one long word across */
	  MOVLrm(tmp,0,dBase,0,0);
	  SUBQrr(four,sBase);		/* Decrement pointers */
	  SUBQrr(four,dBase);
	  SUBLrr(four,cx);
	  TESTQrr(cx,cx);		/* Are there any more? */
	  JNZm(loop);			/* jump to loop */
	}

	// set up closure and return address
	MOVQrr(dBase,_ESP);		/* cannot use SP for base */

	useLbl(lbl,moveRef,jit_movi_p(JIT_REXTMP,lbl),cxt);
	PUSHQr(JIT_REXTMP);		/* The first argument */
	jit_ldr_p(JIT_REXTMP,JIT_REXTMP);
	PUSHQr(oldRtn);			/* and the old return address */

	JMPsr(JIT_REXTMP);		/* jump to code */
      });
    return;
  }

  case CallClbl:{
    insPo lbl = ins->o.oneLbl.lbl;

    tryJit({
	jit_insn *currPc = jit_get_label();
	jit_finish(Null);
	useLbl(lbl,callRef,currPc,cxt);
      });
    return;
  }

  case CallLib:{
    libFun fn = (libFun)ins->o.oneBox.bx;

    tryJit({
	jit_finish(fn);
      });
    return;
  }

  case EnterFun:{
    integer size = ins->o.oneInt.ix;

    tryJit({
	_jitl.nextarg_getfp = _jitl.nextarg_geti = _jitl.alloca_offset = 0;

	PUSHQr(_EBP); 
	MOVQrr(_ESP, _EBP);
	if(size!=0)
	  SUBQir(size, _ESP);

	MOVQmr(ENV_ARG_OFFSET,_EBP,0,0,ENV_REG);
      });
    return;
  }

  case EnterCFun:{
    integer size = ins->o.oneInt.ix;

    tryJit({
	_jitl.nextarg_getfp = _jitl.nextarg_geti = _jitl.alloca_offset = 0;
	PUSHQr(_R12);
	PUSHQr(_R13);
	PUSHQr(_EBP);
	MOVQrr(_ESP, _EBP);
	if(size!=0)
	  SUBQir(size, _ESP);
      });
    return;
  }

  case Rtn:
    tryJit({retrn();});
    return;

  case RtnC:
    tryJit({retrnC();});
    return;

  case Ret:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	if(reg!=JIT_RET)
	  jit_movr_p(JIT_RET,reg);
	retrn();
      });
    return;
  }

  case RetI:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	if(reg!=JIT_RET)
	  jit_movr_l(JIT_RET,reg);
	retrn();
      });
    return;
  }

  case RetL:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	if(reg!=JIT_RET)
	  jit_movr_l(JIT_RET,reg);
	retrn();
      });
    return;
  }

  case RetD:{
    int reg = fpRegs[ins->o.oneFpReg.reg];

    tryJit({
	if(reg!=JIT_FPRET)
	  jit_movr_d(JIT_FPRET,reg);
	retrn();
      });
    return;
  }

  case RetC:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	if(reg!=JIT_RET)
	  jit_movr_l(JIT_RET,reg);
	retrnC();
      });
    return;
  }

  case RetCD:{
    int reg = fpRegs[ins->o.oneFpReg.reg];

    tryJit({
	if(reg!=JIT_FPRET)
	  jit_movr_d(JIT_FPRET,reg);
	retrnC();
      });
    return;
  }

  case Jmp:{
    insPo tgt = ins->o.oneLbl.lbl;
    
    tryJit(useLbl(tgt,codeRef,jit_jmpi(jit_forward()),cxt));
    return;
  }

  case JmpR:{
    int tgt = intRegs[ins->o.oneReg.reg];

    tryJit(JMPsr(tgt));
    return;
  }

  case Unwind:{
    //    int reg = intRegs[ins->o.oneReg.reg];
    int tmp1 = tmpRegs[0];
    int tmp2 = tmpRegs[1];
    int pc = tmpRegs[2];
    int env = intRegs[ENV];

    // Unwind the stack until we get a catch block
    tryJit({
	MOVQir((unsigned long)_jit.x.pc,pc); /* pick up current PC */
	jit_insn *loop = _jit.x.pc;
	MOVQmr(ENV_ARG_OFFSET,FP,0,0,env);
	MOVQmr(0,env,0,0,tmp1);  // get the code block
	MOVQmr(CATCH_OFFSET,tmp1,0,0,tmp1); // get to the catch blocks
	TESTQrr(tmp1,tmp1);		   /* Are there any catch blocks? */
	JZm(Null);			   /* will be jump to outer loop */
	jit_insn *ref0 = _jit.x.pc;
	jit_insn *tblLoop = _jit.x.pc;
	MOVQmr(0,tmp1,0,0,tmp2); // look at the first/next catch
	TESTQrr(tmp2,tmp2);
	JZm(Null);			   /* will be jump to unwind loop */
	jit_insn *ref1 = _jit.x.pc;
	CMPQrr(pc,tmp2);		/* test the range of the catch block */
	JAm(Null);			/* pc is smaller than block start */
	jit_insn *ref3 = _jit.x.pc;
	MOVQmr(POINTER_SIZE,tmp1,0,0,tmp2);
	CMPQrr(tmp2,pc);		/* test upper range */
	JAm(Null);			/* pc is bigger than block end */
	jit_insn *ref2 = _jit.x.pc;
	
	MOVQmr(2*POINTER_SIZE,tmp1,0,0,pc); /* new pc */
	JMPsr(pc);			    /* jump to recovery code */

	jit_patch_at(ref2,_jit.x.pc);
	jit_patch_at(ref3,_jit.x.pc);
	
	LEAQmr(3*POINTER_SIZE,tmp1,0,0,tmp1);	/* look at next table entry */
	JMPm((unsigned long)tblLoop);

	jit_patch_at(ref0,_jit.x.pc);
	jit_patch_at(ref1,_jit.x.pc);

	MOVQmr(RTN_OFFSET,FP,0,0,pc);
	MOVQmr(0,FP,0,0,FP);		/* unwind a bit more of the stack */
	JMPm((unsigned long)loop);	/* try again */
      });
    
    return;
  }

  case Case:{
    int reg = intRegs[ins->o.threeRegIntLbl.reg];
    insPo cases = ins->o.threeRegIntLbl.lbl;

    tryJit({
	MOVQrr(reg,T_REG);
	MOVQmr(0,reg,0,0,JIT_R0);
	MOVQmr(CASE_OFFSET,JIT_R0,0,0,JIT_R0);
	useLbl(cases,moveRef,jit_movi_p(JIT_REXTMP,jit_forward()),cxt);
	MOVQmr(0,JIT_REXTMP,JIT_R0,1,JIT_R0);
	JMPsr(JIT_R0);			// That was the case jump!
      });
    return;
  }

  case Move:{
    int dreg =intRegs[ins->o.twoRegReg.reg1];
    int sreg =intRegs[ins->o.twoRegReg.reg2];
    tryJit({MOVQrr(sreg,dreg);});
    return;
  }
  case MoveDD:{
    int dreg =fpRegs[ins->o.twoFpRegFpReg.reg1];
    int sreg =fpRegs[ins->o.twoFpRegFpReg.reg2];
    tryJit(MOVSDrr(sreg,dreg));
    return;
  }
  case MoveC:{
    int reg = intRegs[ins->o.twoRegInteger.reg];
    integer ix = ins->o.twoRegInteger.ix;
    tryJit({MOVWir(ix,reg);});
    return;
  }
  case MoveI:{
    int reg = intRegs[ins->o.twoRegInteger.reg];
    integer ix = ins->o.twoRegInteger.ix;
    tryJit({MOVLir(ix,reg);});
    return;
  }
  case MoveL:{
    int reg = intRegs[ins->o.twoRegInteger.reg];
    integer ix = ins->o.twoRegInteger.ix;
    tryJit({MOVQir(ix,reg);});
    return;
  }
  case MoveD:{
    int reg = fpRegs[ins->o.twoFpRegDouble.reg];
    double d = ins->o.twoFpRegDouble.d;
    tryJit({jit_movi_d(reg,d);});
    return;
  }

  case MoveBx:{
    int reg = intRegs[ins->o.twoRegBox.reg];
    integer ix = (integer)ins->o.twoRegBox.bx;
    tryJit({MOVQir(ix,reg);});
    return;
  }

  case MoveLbl:{
    int reg = intRegs[ins->o.twoRegLbl.reg];
    insPo lbl = ins->o.twoRegLbl.lbl;
    tryJit({
	useLbl(lbl,moveRef,jit_movi_p(reg,lbl),cxt);
      });
    return;
  }

  case Ld:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg1];
    int sreg = intRegs[ins->o.threeRegRegInt.reg2];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVQmr(off,sreg,0,0,dreg);
      });
    return;
  }

  case LdC:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg1];
    int sreg = intRegs[ins->o.threeRegRegInt.reg2];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVWmr(off,sreg,0,0,dreg);
      });
    return;
  }

  case LdI:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg1];
    int sreg = intRegs[ins->o.threeRegRegInt.reg2];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVLmr(off,sreg,0,0,dreg);
      });
    return;
  }

  case LdL:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg1];
    int sreg = intRegs[ins->o.threeRegRegInt.reg2];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVQmr(off,sreg,0,0,dreg);
      });
    return;
  }

  case LdD:{
    int dreg = intRegs[ins->o.threeFpRegRegInt.reg1];
    int sreg = intRegs[ins->o.threeFpRegRegInt.reg2];
    int off = ins->o.threeFpRegRegInt.ix;

    tryJit({
	jit_ldxi_d(dreg,sreg,off);
      });
    return;
  }

  case St:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg2];
    int sreg = intRegs[ins->o.threeRegRegInt.reg1];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVQrm(sreg,off,dreg,0,0);
      });
    return;
  }

  case StC:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg2];
    int sreg = intRegs[ins->o.threeRegRegInt.reg1];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVWrm(sreg,off,dreg,0,0);
      });
    return;
  }

  case StI:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg2];
    int sreg = intRegs[ins->o.threeRegRegInt.reg1];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVLrm(sreg,off,dreg,0,0);
      });
    return;
  }

  case StL:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg2];
    int sreg = intRegs[ins->o.threeRegRegInt.reg1];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVQrm(sreg,off,dreg,0,0);
      });
    return;
  }

  case StD:{
    int dreg = intRegs[ins->o.threeRegRegInt.reg2];
    int sreg = intRegs[ins->o.threeRegRegInt.reg1];
    int off = ins->o.threeRegRegInt.ix;

    tryJit({
	MOVSDrm(sreg,off,dreg,0,0);
      });
    return;
  }

  case StLbl:{
    int dreg = intRegs[ins->o.threeRegIntLbl.reg];
    int off = ins->o.threeRegIntLbl.ix;
    insPo lbl = ins->o.threeRegIntLbl.lbl;

    int tmp = tmpRegs[0];

    tryJit({
	useLbl(lbl,moveRef,jit_movi_p(tmp,lbl),cxt);
	MOVQrm(tmp,off,dreg,0,0);
      });
    return;
  }

  case HpChk:{
    int size = ins->o.twoIntLbl.ix;
    insPo fail = ins->o.twoIntLbl.lbl;

    tryJit({
	MOVQir((unsigned long)&heap.curr,JIT_REXTMP);
	MOVQmr(0,JIT_REXTMP,0,0,JIT_REXTMP);

	MOVQir((unsigned long)&heap.limit,JIT_R0);
	MOVQmr(0,JIT_R0,0,0,JIT_R0);
	ADDQir(size,JIT_R0);

	CMPQrr(JIT_REXTMP,JIT_R0);
	JGEm(fail);

	useLbl(fail,codeRef,_jit.x.pc,cxt);
      });
    
    return;
  }

  case AllocH:{
    int reg = intRegs[ins->o.threeRegIntLbl.reg];
    int amnt = ins->o.threeRegIntLbl.ix;
    insPo fail = ins->o.threeRegIntLbl.lbl;

    tryJit({
	/* pick up current heap */
	int tmp1 = tmpRegs[0];
	int tmp2 = T_REG;
	int tmp3 = tmpRegs[1];

	MOVQir((unsigned long)&heap.curr,tmp1);
	MOVQmr(0,tmp1,0,0,reg);

	LEAQmr(amnt,reg,0,0,tmp2);	/* address of next heap block */

	// heap limit is near heap.curr
	int delta = ((void*)&heap.limit-(void*)&heap.curr);
	MOVQmr(delta,tmp1,0,0,tmp3);	/* pick up heap limit */

	CMPQrr(tmp3,tmp2);		/* compare against limit */
	JGm(jit_forward());
	useLbl(fail,codeRef,_jit.x.pc,cxt);

	MOVQrm(tmp2,0,tmp1,0,0);	/* set new current heap */
      });

    return;
  }

  case Gc:{
    int amnt = ins->o.oneInt.ix;

    tryJit({
	MOVQir(amnt,_RDX);		/* amount to find is 3rd argument */
	MOVQir(0,_RSI);			/* prepare pointer to call site */
	jit_insn *sitref = _jit.x.pc;
	MOVQrr(_RBP,_RDI);		/* start with this frame */
	CALLm((long)collect);
	jit_patch_movi(sitref,_jit.x.pc); /* address of next instruction */
      });

    return;
  }

  case PlantFwd:{
    int treg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo fwd = ins->o.threeRegRegLbl.lbl;
    int tmpReg = tmpRegs[0];

    tryJit({
	MOVQir(jit_forward(),tmpReg);
	useLbl(fwd,moveRef,_jit.x.pc,cxt);
	MOVQrm(tmpReg,0,sreg,0,0);
	MOVQrm(treg,FORWARD_OFFSET,sreg,0,0);
      });

    return;
  }

  case RetNext:{
    int reg = intRegs[ins->o.twoRegInteger.reg];
    int offset = ins->o.twoRegInteger.ix;
    tryJit({
	jit_addi_p(JIT_RET,reg,offset);
	retrnC();
      });

    return;
  }

  case Evac:{
    int reg = intRegs[ins->o.twoRegInteger.reg];
    int offset = ins->o.twoRegInteger.ix;
    tryJit({
	jit_ldxi_p(JIT_R0,reg,offset);
	jit_prepare(1);
	jit_pusharg_p(JIT_R0);
	jit_ldr_p(JIT_R0,JIT_R0);
	MOVQmr(GC_EVAC_OFFSET,JIT_R0,0,0,JIT_R0);
	//      jit_ldxi_p(JIT_R0,JIT_R0,GC_EVAC_OFFSET);
	jit_finishr(JIT_R0);
	jit_retval_p(JIT_R0);
	jit_stxi_p(offset,reg,JIT_R0);
      });
    return;
  }

  case L2F:{
    int dreg = fpRegs[ins->o.twoFpRegReg.reg1];
    int sreg = intRegs[ins->o.twoFpRegReg.reg2];

    tryJit({
	jit_extr_l_d(dreg,sreg);
      });
    return;
  }

  case F2L:{
    int sreg = fpRegs[ins->o.twoFpRegReg.reg1];
    int dreg = intRegs[ins->o.twoFpRegReg.reg2];

    tryJit({
	jit_roundr_d_l(dreg,sreg);
      });
    return;
  }

  case C2I:{
    int reg = intRegs[ins->o.oneReg.reg];
    tryJit({
	ANDQir(reg,0xffff);
      });
    return;
  }

  case I2L:{
    int reg = intRegs[ins->o.oneReg.reg];
    tryJit({
	SHLQir(32,reg);
	SARQir(32,reg);
      });
    return;
  }

  case AddC:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	ADDWrr(sreg,dreg);
      });
    return;
  }

  case AddI:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	ADDLrr(sreg,dreg);
      });
    return;
  }

  case AddL:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	ADDQrr(sreg,dreg);
      });
    return;
  }

  case AddD:{
    int dreg = fpRegs[ins->o.twoFpRegFpReg.reg1];
    int sreg = fpRegs[ins->o.twoFpRegFpReg.reg2];

    tryJit({
	jit_addr_d(dreg,dreg,sreg);
      });
    return;
  }

  case IncC:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	INCWr(reg);
      });
    return;
  }

  case IncI:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	INCLr(reg);
      });
    return;
  }

  case IncL:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	INCQr(reg);
      });
    return;
  }

  case SubC:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SUBWrr(sreg,dreg);
      });
    return;
  }

  case SubI:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SUBLrr(sreg,dreg);
      });
    return;
  }

  case SubL:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SUBQrr(sreg,dreg);
      });
    return;
  }

  case SubD:{
    int dreg = fpRegs[ins->o.twoFpRegFpReg.reg1];
    int sreg = fpRegs[ins->o.twoFpRegFpReg.reg2];

    tryJit({
	jit_addr_d(dreg,dreg,sreg);
      });
    return;
  }

  case DecC:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	DECWr(reg);
      });
    return;
  }

  case DecI:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	DECLr(reg);
      });
    return;
  }

  case DecL:{
    int reg = intRegs[ins->o.oneReg.reg];

    tryJit({
	DECQr(reg);
      });
    return;
  }

  case MulC:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	IMULWrr(sreg,dreg);
      });
    return;
  }

  case MulI:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	IMULLrr(sreg,dreg);
      });
    return;
  }

  case MulL:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	IMULQrr(sreg,dreg);
      });
    return;
  }

  case MulD:{
    int dreg = fpRegs[ins->o.twoFpRegFpReg.reg1];
    int sreg = fpRegs[ins->o.twoFpRegFpReg.reg2];

    tryJit({
	jit_mulr_d(dreg,dreg,sreg);
      });

    return;
  }

  case DivC:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	if(dreg!=_RDX)
	  MOVWrr(dreg,_RDX);
	MOVWrr(_RDX,_RAX);
	SARWir(15, _RDX); // extract sign
	IDIVWr(sreg);
	if(dreg!=_RAX)
	  MOVWrr(_RAX,dreg);
      });
    return;
  }

  case DivI:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	if(dreg!=_RDX)
	  MOVLrr(dreg,_RDX);
	MOVLrr(_RDX,_RAX);
	SARLir(31, _RDX); // extract sign
	IDIVLr(sreg);
	if(dreg!=_RAX)
	  MOVLrr(_RAX,dreg);
      });
    return;
  }

  case DivL:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	if(dreg!=_RDX)
	  MOVQrr(dreg,_RDX);
	MOVQrr(_RDX,_RAX);
	SARQir(63, _RDX); // extract sign
	IDIVQr(sreg);
	if(dreg!=_RAX)
	  MOVQrr(_RAX,dreg);
      });
    return;
  }

  case DivD:{
    int dreg = fpRegs[ins->o.twoFpRegFpReg.reg1];
    int sreg = fpRegs[ins->o.twoFpRegFpReg.reg2];

    tryJit({
	jit_divr_d(dreg,dreg,sreg);
      });

    return;
  }

  case RemC:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	if(dreg!=_RDX)
	  MOVWrr(dreg,_RDX);
	MOVWrr(_RDX,_RAX);
	SARWir(15, _RDX); // extract sign
	IDIVWr(sreg);
	if(dreg!=_RDX)
	  MOVWrr(_RDX,dreg);
      });
    return;
  }

  case RemI:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	if(dreg!=_RDX)
	  MOVLrr(dreg,_RDX);
	MOVLrr(_RDX,_RAX);
	SARLir(31, _RDX); // extract sign
	IDIVLr(sreg);
	if(dreg!=_RDX)
	  MOVLrr(_RDX,dreg);
      });
    return;
  }

  case RemL:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	if(dreg!=_RDX)
	  MOVQrr(dreg,_RDX);
	MOVQrr(_RDX,_RAX);
	SARQir(63, _RDX); // extract sign
	IDIVQr(sreg);
	if(dreg!=_RDX)
	  MOVQrr(_RDX,dreg);
      });
    return;
  }

  case LeftC:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SHLWrr(sreg,dreg);
      });
    return;
  }

  case LeftI:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SHLLrr(sreg,dreg);
      });
    return;
  }

  case LeftL:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SHLQrr(sreg,dreg);
      });
    return;
  }

  case LsL:{
    int reg = intRegs[ins->o.twoRegInteger.reg];
    int amnt = ins->o.twoRegInteger.ix;

    tryJit({
	SHLLir(amnt,reg);
      });
    return;
  }

  case RightC:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SHRWrr(sreg,dreg);
      });
    return;
  }

  case RightI:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SHRLrr(sreg,dreg);
      });
    return;
  }

  case RightL:{
    int dreg = intRegs[ins->o.twoRegReg.reg1];
    int sreg = intRegs[ins->o.twoRegReg.reg2];

    tryJit({
	SHRQrr(sreg,dreg);
      });
    return;
  }

  case RsL:{
    int reg = intRegs[ins->o.twoRegInteger.reg];
    int amnt = ins->o.twoRegInteger.ix;

    tryJit({
	SHRQir(amnt,reg);
      });
    return;
  }

  case BzC:{
    int reg = intRegs[ins->o.twoRegLbl.reg];
    insPo tgt = ins->o.twoRegLbl.lbl;

    tryJit({
	TESTWrr(reg,reg);
	JZm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BzI:{
    int reg = intRegs[ins->o.twoRegLbl.reg];
    insPo tgt = ins->o.twoRegLbl.lbl;

    tryJit({
	TESTLrr(reg,reg);
	JZm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BzL:{
    int reg = intRegs[ins->o.twoRegLbl.reg];
    insPo tgt = ins->o.twoRegLbl.lbl;

    tryJit({
	TESTQrr(reg,reg);
	JZm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BzD:{
    int reg = fpRegs[ins->o.twoFpRegLbl.reg];
    int tmp = ins->o.twoFpRegLbl.reg==FPR0 ?
      fpRegs[FPR1]:
      fpRegs[FPR0];
    insPo tgt = ins->o.twoFpRegLbl.lbl;

    tryJit({
	jit_movi_d(tmp,0);
	jit_beqr_d(jit_forward(),reg,tmp);
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BnzC:{
    int reg = intRegs[ins->o.twoRegLbl.reg];
    insPo tgt = ins->o.twoRegLbl.lbl;

    tryJit({
	TESTWrr(reg,reg);
	JNZm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BnzI:{
    int reg = intRegs[ins->o.twoRegLbl.reg];
    insPo tgt = ins->o.twoRegLbl.lbl;

    tryJit({
	TESTLrr(reg,reg);
	JNZm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BnzL:{
    int reg = intRegs[ins->o.twoRegLbl.reg];
    insPo tgt = ins->o.twoRegLbl.lbl;

    tryJit({
	TESTQrr(reg,reg);
	JNZm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BnzD:{
    int reg = fpRegs[ins->o.twoFpRegLbl.reg];
    int tmp = ins->o.twoFpRegLbl.reg==FPR0 ?
      fpRegs[FPR1]:
      fpRegs[FPR0];
    insPo tgt = ins->o.twoFpRegLbl.lbl;

    tryJit({
	jit_movi_d(tmp,0.0);
	jit_bner_d(0,reg,tmp);
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Beq:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	useLbl(tgt,codeRef,jit_beqr_p(jit_forward(),dreg,sreg),cxt);
      });
    return;
  }

  case BeqC:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPWrr(dreg,sreg);
	JEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BeqI:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPLrr(dreg,sreg);
	JEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BeqL:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPQrr(dreg,sreg);
	JEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BeqD:{
    int dreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg1];
    int sreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg2];
    insPo tgt = ins->o.threeFpRegFpRegLbl.lbl;

    tryJit({
	jit_beqr_d(jit_forward(),dreg,sreg);
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Bne:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	useLbl(tgt,codeRef,jit_bner_p(jit_forward(),dreg,sreg),cxt);
      });
    return;
  }

  case BneC:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPWrr(dreg,sreg);
	JNEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BneI:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPLrr(dreg,sreg);
	JNEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BneL:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPQrr(dreg,sreg);
	JNEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BneD:{
    int dreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg1];
    int sreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg2];
    insPo tgt = ins->o.threeFpRegFpRegLbl.lbl;


    tryJit({
	UCOMISDrr(dreg,sreg);
	JNEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Blt:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	useLbl(tgt,codeRef,jit_bltr_p(jit_forward(),dreg,sreg),cxt);
      });
    return;
  }

  case BltC:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPWrr(sreg,dreg);
	JLm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BltI:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPLrr(sreg,dreg);
	JLm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BltL:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPQrr(sreg,dreg);
	JLm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BltD:{
    int dreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg1];
    int sreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg2];
    insPo tgt = ins->o.threeFpRegFpRegLbl.lbl;

    tryJit({
	jit_bltr_d(jit_forward(),dreg,sreg);
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Ble:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	useLbl(tgt,codeRef,jit_bler_p(jit_forward(),dreg,sreg),cxt);
      });
    return;
  }

  case BleC:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPWrr(sreg,dreg);
	JLEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BleI:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPLrr(sreg,dreg);
	JLEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BleL:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPQrr(sreg,dreg);
	JLEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BleD:{
    int dreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg1];
    int sreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg2];
    insPo tgt = ins->o.threeFpRegFpRegLbl.lbl;

    tryJit({
	jit_bler_d(jit_forward(),dreg,sreg);
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Bge:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	useLbl(tgt,codeRef,jit_bger_p(jit_forward(),dreg,sreg),cxt);
      });
    return;
  }

  case BgeC:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPWrr(sreg,dreg);
	JGEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BgeI:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPLrr(sreg,dreg);
	JGEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BgeL:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPQrr(sreg,dreg);
	JGEm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BgeD:{
    int dreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg1];
    int sreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg2];
    insPo tgt = ins->o.threeFpRegFpRegLbl.lbl;

    tryJit({
	jit_bger_d(jit_forward(),dreg,sreg);
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Bgt:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	useLbl(tgt,codeRef,jit_bgtr_p(jit_forward(),dreg,sreg),cxt);
      });
    return;
  }

  case BgtC:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPWrr(sreg,dreg);
	JGm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BgtI:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPLrr(sreg,dreg);
	JGm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BgtL:{
    int dreg = intRegs[ins->o.threeRegRegLbl.reg1];
    int sreg = intRegs[ins->o.threeRegRegLbl.reg2];
    insPo tgt = ins->o.threeRegRegLbl.lbl;

    tryJit({
	CMPQrr(sreg,dreg);
	JGm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case BgtD:{
    int dreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg1];
    int sreg = fpRegs[ins->o.threeFpRegFpRegLbl.reg2];
    insPo tgt = ins->o.threeFpRegFpRegLbl.lbl;

    tryJit({
	jit_bgtr_d(jit_forward(),dreg,sreg);
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Bcs:{				/* branch on carry set */
    insPo tgt = ins->o.oneLbl.lbl;
    tryJit({
	JCm(0);
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Bcc:{				/* branch on carry clear */
    insPo tgt = ins->o.oneLbl.lbl;
    tryJit({
	JNCm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Bov:{				/* branch on overflow set */
    insPo tgt = ins->o.oneLbl.lbl;
    tryJit({
	JOm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case Boc:{				/* branch on overflow clear */
    insPo tgt = ins->o.oneLbl.lbl;
    tryJit({
	JNOm(jit_forward());
	useLbl(tgt,codeRef,_jit.x.pc,cxt);
      });
    return;
  }

  case AlignTo:
    align(cxt,ins->o.oneInt.ix);
    
    return;

  case ConstI:{
    integer ix = ins->o.oneInt.ix;

    align(cxt,INTEGER_SIZE);
    tryJit({
	*_jit.x.ul_pc++ = (unsigned long)ix;
      });
    return;
  }

  case ConstL:{
    integer ix = ins->o.oneInt.ix;

    align(cxt,LONG_SIZE);
    tryJit({
	*_jit.x.ul_pc++ = (unsigned long)ix;
      });
    return;
  }

  case ConstD:{
    double d = ins->o.oneDbl.d;

    align(cxt,DOUBLE_SIZE);

    tryJit({
	double *dPc = (double*)_jit.x.pc;
	*dPc++ = d;
	_jit.x.pc = (jit_insn*)dPc;
      });

    return;
  }

  case ConstP:{
    align(cxt,POINTER_SIZE);
    insPo lbl = ins->o.oneLbl.lbl;

    tryJit({
	useLbl(lbl,constRef,_jit.x.pc,cxt);
	*_jit.x.ul_pc++ = (unsigned long)lbl;
      });

    return;
  }

    /*  case ConstS:{
	constantPo conn = cxt->mtd->pool[ins->o.oneInt.ix];
	align(cxt,POINTER_SIZE);

	tryJit({
	void **dest = (void**)_jit.x.ul_pc;
	*dest++ = &conn->data;
	_jit.x.ul_pc = (unsigned long*)dest;
	});

	return;
	}
    */

  case DefineLbl:{
    if(cxt->mode==counting){
      hashPut(cxt->lbls,ins,(void*)cxt->pc);
    }
    return;
  }
  }
#undef _jit
}

static void useLbl(insPo lbl,fixupType type, jit_insn *ref,asmCxtPo cxt)
{
#define _jit          (cxt->state)
  if(cxt->mode==generating){
    long pc = (long)hashGet(cxt->lbls,lbl);

    assert(pc!=0);

    jit_insn *tgt = (jit_insn *)(cxt->buffer+pc);

    switch(type){
    case codeRef:
      jit_patch_at(ref,tgt);
      break;
    case moveRef:
      jit_patch_movi(ref,tgt);
      break;
    case constRef:
      *_jit.x.ul_pc = (unsigned long)tgt;
      break;
    case callRef:{
      jit_insn *currPc = jit_get_label();
      _jit.x.pc = ref;
      jit_finish(tgt);
      _jit.x.pc = currPc;
      break;
    }
    }
  }
#undef _jit
}

logical is16bit(integer ix)
{
  return ix>=-32768 && ix<32767;
}

void outOfMemError()
{
  outMsg(logFile,"Heap space exhausted\n");
  flushOut();
  exit(99);
}
