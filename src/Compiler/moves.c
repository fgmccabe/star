/*
 * Generate code for moving stuff around
 */

#include "compiler.h"
#include "dict.h"

#include "compile.h"
#include "type.h"
#include "memo.h"

retCode loadReg(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  switch(src->kind){
  case rawChar:{
    Register reg = *(Register*)cl;

    switch(src->where){
    case basedVar:
      ALdC(code,reg,src->base,src->l.off);
      return Ok;
    case registr:
      if(reg!=src->l.reg)
	AMove(code,reg,src->l.reg);
      return Ok;
    case literal:
      AMoveC(code,reg,src->l.ix);
      return Ok;
    case fixed:
      AMoveBx(code,reg,src->l.bx);
      return Ok;
    case label:
      reportError(loc,"invalid source for loadReg");
      return Error;
    }
  }
  case rawInt:{
    Register reg = *(Register*)cl;

    switch(src->where){
    case basedVar:
      ALdI(code,reg,src->base,src->l.off);
      return Ok;
    case registr:
      if(reg!=src->l.reg)
	AMove(code,reg,src->l.reg);
      return Ok;
    case literal:
      AMoveI(code,reg,src->l.ix);
      return Ok;
    case fixed:
      AMoveBx(code,reg,src->l.bx);
      return Ok;
    case label:
      reportError(loc,"invalid source for loadReg");
      return Error;
    }
  }

  case rawLong:{
    Register reg = *(Register*)cl;

    switch(src->where){
    case basedVar:
      ALdL(code,reg,src->base,src->l.off);
      return Ok;
    case registr:
      if(reg!=src->l.reg)
	AMove(code,reg,src->l.reg);
      return Ok;
    case literal:
      AMoveL(code,reg,src->l.ix);
      return Ok;
    case fixed:
      AMoveBx(code,reg,src->l.bx);
      return Ok;
    case label:
      reportError(loc,"invalid source for loadReg");
      return Error;
    }
  }
  case rawFloat:{
    FpRegister reg = *(FpRegister*)cl;

    switch(src->where){
    case basedVar:
      ALdD(code,reg,src->base,src->l.off);
      return Ok;
    case registr:
      if(reg!=src->l.fpReg)
	AMoveDD(code,reg,src->l.fpReg);
      return Ok;
    case literal:
      AMoveD(code,reg,src->l.d);
      return Ok;
    case fixed:
      AMoveBx(code,reg,src->l.bx);
      return Ok;
    case label:
      reportError(loc,"invalid source for loadReg");
      return Error;
    }
  }

  case general:{
    Register reg = *(Register*)cl;

    switch(src->where){
    case basedVar:
      ALd(code,reg,src->base,src->l.off);
      return Ok;
    case registr:
      if(reg!=src->l.reg)
	AMove(code,reg,src->l.reg);
      return Ok;
    case literal:
      AMoveLbl(code,reg,src->l.bx);
      return Ok;
    case fixed:
      AMoveBx(code,reg,src->l.bx);
      return Ok;
    case label:
      AMoveLbl(code,reg,src->l.lit);
      return Ok;
    }
  }
  default:
    reportError(loc,"invalid source for loadReg");
    return Error;
  }
}

retCode loadFpReg(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  FpRegister reg = *(FpRegister*)cl;

  switch(src->kind){
  case rawInt:{
    switch(src->where){
    case basedVar:
      ALdI(code,R0,src->base,src->l.off);
      AL2F(code,reg,R0);
      return Ok;
    case registr:
      AL2F(code,reg,src->l.reg);
      return Ok;
    case literal:
      AMoveD(code,reg,(double)src->l.ix);
      return Ok;
    case fixed:
    case label:
      reportError(loc,"invalid source for loadReg");
      return Error;
    }
  }

  case rawLong:{
    switch(src->where){
    case basedVar:
      ALdL(code,R0,src->base,src->l.off);
      AL2F(code,reg,R0);
      return Ok;
    case registr:
      AL2F(code,reg,src->l.reg);
      *(Register*)cl = src->l.reg;	/* just say where the register is */
      return Ok;
    case literal:
      AMoveD(code,reg,(double)src->l.ix);
      return Ok;
    case fixed:
    case label:
      reportError(loc,"invalid source for loadReg");
      return Error;
    }
  }

  case rawFloat:{
    switch(src->where){
    case basedVar:
      ALdD(code,reg,src->base,src->l.off);
      return Ok;
    case registr:
      if(reg!=src->l.fpReg)
      AMoveDD(code,reg,src->l.fpReg);
      return Ok;
    case literal:
      AMoveD(code,reg,src->l.d);
      return Ok;
    case fixed:
    case label:
      reportError(loc,"invalid source for loadReg");
      return Error;
    }
  }

  default:
    reportError(loc,"invalid source for loadFpReg");
    return Error;
  }
}


logical isJumpCont(contFun cont,void *cl)
{
  if(cont==returnCont || cont==jumpCont|| cont==memoCont || cont==rtnCont)
    return True;
  else if(cont==comboCont){
    comboPo combo = (comboPo)cl;
    return isJumpCont(combo->cont2,combo->cl2);
  }
  else
    return False;
}

retCode jumpTarget(assemPo code,lPo lbl)
{
  insPo last = endIns(code);
  if(isJumpIns(last,lbl))
    removeIns(code,last);		/* remove redundant jump */

  defineLbl(code,lbl);
  return Ok;
}

retCode rtnCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  ARtn(code);
  return Ok;
}

retCode CrtnCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  ARtnC(code);
  return Ok;
}

retCode returnCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  switch(src->kind){
  case rawChar:
    switch(src->where){
    case basedVar:
      ALdC(code,R0,src->base,src->l.off);
      ARet(code,R0);
      return Ok;
    case registr:
      ARet(code,src->l.reg);
      return Ok;
    case literal:
      AMoveC(code,R0,src->l.ix);
      ARet(code,R0);
      return Ok;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      return Ok;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

  case rawInt:
    switch(src->where){
    case basedVar:
      ALdI(code,R0,src->base,src->l.off);
      ARet(code,R0);
      return Ok;
    case registr:
      ARet(code,src->l.reg);
      return Ok;
    case literal:
      AMoveI(code,R0,src->l.ix);
      ARet(code,R0);
      return Ok;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      return Ok;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

  case rawLong:
    switch(src->where){
    case basedVar:
      ALdL(code,R0,src->base,src->l.off);
      ARet(code,R0);
      return Ok;
    case registr:
      ARet(code,src->l.reg);
      return Ok;
    case literal:
      AMoveL(code,R0,src->l.ix);
      ARet(code,R0);
      return Ok;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      return Ok;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

  case rawFloat:
    switch(src->where){
    case basedVar:
      ALdD(code,FPR0,src->base,src->l.off);
      ARetD(code,FPR0);
      return Ok;
    case registr:
      ARetD(code,src->l.fpReg);
      return Ok;
    case literal:
      AMoveD(code,FPR0,src->l.d);
      ARetD(code,FPR0);
      return Ok;
    case fixed:
    default:
      reportError(loc,"invalid source for return");
      return Error;
    }

  case general:
    switch(src->where){
    case basedVar:
      ALd(code,R0,src->base,src->l.off);
      ARet(code,R0);
      return Ok;
    case registr:
      ARet(code,src->l.reg);
      return Ok;
    case literal:
      AMoveBx(code,R0,src->l.bx);
      ARet(code,R0);
      return Ok;
    case label:
      AMoveLbl(code,R0,src->l.lit);
      ARet(code,R0);
      return Ok;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      ARet(code,R0);
      return Ok;
    }
  }
  return Error;
}

retCode memoCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  switch(src->kind){
  case rawChar:{
    int reg = R0;
    int uReg = R1;

    switch(src->where){
    case basedVar:
      ALdC(code,R0,src->base,src->l.off);
      break;

    case registr:{
      reg = src->l.reg;
      uReg = reg==R0?R1:R0;
      break;
    }
    case literal:
      AMoveC(code,R0,src->l.ix);
      break;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      break;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

    AStC(code,ENV,POINTER_SIZE,reg);	/* store the value in the 1st free */
    AMoveLbl(code,uReg,findKFun(loc,src->type));
    ASt(code,ENV,0,uReg);		/* overwrite fun code to K function */
    ARet(code,reg);
    return Ok;
  }

  case rawInt:{
    int reg = R0;
    int uReg = R1;

    switch(src->where){
    case basedVar:
      ALdI(code,R0,src->base,src->l.off);
      break;
    case registr:
      reg = src->l.reg;
      uReg = src->l.reg==R0?R1:R0;
      break;
    case literal:
      AMoveI(code,R0,src->l.ix);
      break;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      break;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

    AStI(code,ENV,POINTER_SIZE,reg);	/* store the value in the 1st free */
    AMoveLbl(code,uReg,findKFun(loc,src->type));
    ASt(code,ENV,0,uReg);		/* overwrite fun code to K function */
    ARet(code,reg);
    return Ok;
  }

  case rawLong:{
    int reg = R0;
    int uReg = R1;

    switch(src->where){
    case basedVar:
      ALdL(code,R0,src->base,src->l.off);
      break;
    case registr:
      reg = src->l.reg;
      uReg = src->l.reg==R0?R1:R0;
      break;
    case literal:
      AMoveL(code,R0,src->l.ix);
      break;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      break;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

    AStL(code,ENV,POINTER_SIZE,reg);	/* store the value in the 1st free */
    AMoveLbl(code,uReg,findKFun(loc,src->type));
    ASt(code,ENV,0,uReg);		/* overwrite fun code to K function */
    ARet(code,reg);
    return Ok;
  }

  case rawFloat:{
    int fpReg = FPR0;

    switch(src->where){
    case basedVar:
      ALdD(code,FPR0,src->base,src->l.off);
      break;
    case registr:
      fpReg = src->l.fpReg;
      break;
    case literal:
      AMoveD(code,FPR0,src->l.d);
      break;
    default:
      reportError(loc,"invalid source for return");
      return Error;
    }

    AStD(code,ENV,POINTER_SIZE,fpReg);	/* store the value in the 1st free */
    AMoveLbl(code,R1,findKFun(loc,src->type));
    ASt(code,ENV,0,R1);		/* overwrite fun code to K function */
    ARetD(code,fpReg);
    return Ok;
  }

  case general:{
    int reg = R0;
    int upReg = R1;

    switch(src->where){
    case basedVar:
      ALd(code,R0,src->base,src->l.off);
      break;
    case registr:
      reg = src->l.reg;
      if(reg==R0)
	upReg = R1;
      else
	upReg = R0;
      break;
    case literal:
      AMoveBx(code,R0,src->l.bx);
      break;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      break;
    case label:
      AMoveLbl(code,R0,src->l.lit);
      break;
    }

    AStL(code,ENV,POINTER_SIZE,reg);	/* store the value in the 1st free */
    AMoveLbl(code,upReg,findKFun(loc,src->type));
    ASt(code,ENV,0,upReg);		/* overwrite fun code to K function */
    ARet(code,reg);
    return Ok;
  }
  }
  return Error;
}

retCode returnCCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  switch(src->kind){
  case rawChar:
    switch(src->where){
    case basedVar:
      ALdC(code,R0,src->base,src->l.off);
      ARetC(code,R0);
      return Ok;
    case registr:
      ARetC(code,src->l.reg);
      return Ok;
    case literal:
      AMoveC(code,R0,src->l.ix);
      ARetC(code,R0);
      return Ok;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      ARetC(code,R0);
      return Ok;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

  case rawInt:
    switch(src->where){
    case basedVar:
      ALdI(code,R0,src->base,src->l.off);
      ARetC(code,R0);
      return Ok;
    case registr:
      ARetC(code,src->l.reg);
      return Ok;
    case literal:
      AMoveI(code,R0,src->l.ix);
      ARetC(code,R0);
      return Ok;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      ARetC(code,R0);
      return Ok;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

  case rawLong:
    switch(src->where){
    case basedVar:
      ALdL(code,R0,src->base,src->l.off);
      ARetC(code,R0);
      return Ok;
    case registr:
      ARetC(code,src->l.reg);
      return Ok;
    case literal:
      AMoveL(code,R0,src->l.ix);
      ARetC(code,R0);
      return Ok;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      ARetC(code,R0);
      return Ok;
    case label:
      reportError(loc,"invalid source for return");
      return Error;
    }

  case rawFloat:
    switch(src->where){
    case basedVar:
      ALdD(code,FPR0,src->base,src->l.off);
      ARetCD(code,FPR0);
      return Ok;
    case registr:
      ARetCD(code,src->l.fpReg);
      return Ok;
    case literal:
      AMoveD(code,FPR0,src->l.d);
      ARetCD(code,FPR0);
      return Ok;
    case fixed:
    default:
      reportError(loc,"invalid source for return");
      return Error;
    }

  case general:
    switch(src->where){
    case basedVar:
      ALd(code,R0,src->base,src->l.off);
      ARetC(code,R0);
      return Ok;
    case registr:
      ARetC(code,src->l.reg);
      return Ok;
    case literal:
      AMoveBx(code,R0,src->l.bx);
      ARetC(code,R0);
      return Ok;
    case fixed:
      AMoveBx(code,R0,src->l.bx);
      ARetC(code,R0);
      return Ok;
    case label:
      AMoveLbl(code,R0,src->l.lit);
      ARetC(code,R0);
    }
  }
  return Error;
}

retCode returnMain(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  int retVal = *(int*)cl;
  AMoveI(code,R0,retVal);
  ARetC(code,R0);
  return Ok;
}

retCode assignVar(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  varInfoPo v = (varInfoPo)cl;

  v->inited = True;

  if(src->kind==rawFloat){
    FpRegister fpReg = FPR0;
    loadFpReg(loc,src,&fpReg,code);

    switch(v->kind){
    case rawChar:
    case general:
      reportError(loc,"invalid target for assign");
      return Error;

    case rawInt:{
      Register reg = R0;
      AF2L(code,reg,fpReg);

      switch(v->where){
      case basedVar:
	AStI(code,v->base,v->l.off,reg);
	return Ok;
      case registr:
	AMove(code,v->l.reg,reg);
	return Ok;
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }

    case rawLong:{
      Register reg = R0;
      AF2L(code,reg,fpReg);

      switch(v->where){
      case basedVar:
	AStL(code,v->base,v->l.off,reg);
	return Ok;
      case registr:
	AMove(code,v->l.reg,reg);
	return Ok;
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }

    case rawFloat:{
      switch(v->where){
      case basedVar:
	AStD(code,v->base,v->l.off,fpReg);
	return Ok;
      case registr:
	AMoveDD(code,v->l.fpReg,fpReg);
	return Ok;
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    }
  }
  else{
    switch(v->kind){
    case rawChar:{
      switch(v->where){
      case basedVar:{
	if(src->where==registr)
	  AStC(code,v->base,v->l.off,src->l.reg);
	else{
	  Register reg = R0;
	  loadReg(loc,src,&reg,code);
	  
	  AStC(code,v->base,v->l.off,reg);
	}
	return Ok;
      }
      case registr:{
	Register reg = v->l.reg;
	return loadReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    case rawInt:{
      switch(v->where){
      case basedVar:{
	Register reg = R0;
	loadReg(loc,src,&reg,code);
	AStI(code,v->base,v->l.off,reg);
	return Ok;
      }
      case registr:{
	Register reg = v->l.reg;
	return loadReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    case rawLong:{
      switch(v->where){
      case basedVar:{
	Register reg = R0;
	loadReg(loc,src,&reg,code);
	AStL(code,v->base,v->l.off,reg);
	return Ok;
      }
      case registr:{
	Register reg = v->l.reg;
	return loadReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    case rawFloat:{
      switch(v->where){
      case basedVar:{
	FpRegister reg = FPR0;
	loadFpReg(loc,src,&reg,code);
	AStD(code,v->base,v->l.off,reg);
	return Ok;
      }
      case registr:{
	FpRegister reg = v->l.fpReg;
	return loadFpReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }

    case general:{
      switch(v->where){
      case basedVar:{
	if(src->where==registr)
	  ASt(code,v->base,v->l.off,src->l.reg);
	else{
	  Register reg = R0;
	  loadReg(loc,src,&reg,code);
	  ASt(code,v->base,v->l.off,reg);
	}
	return Ok;
      }
      case registr:{
	Register reg = v->l.reg;
	return loadReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    default:
      reportError(loc,"invalid target for variable");
      return Error;
    }
  }
  return Error;
}

retCode assignResource(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  varInfoPo v = (varInfoPo)cl;

  v->inited = True;

  if(src->kind==rawFloat){
    FpRegister fpReg = FPR0;
    loadFpReg(loc,src,&fpReg,code);

    switch(v->kind){
    case rawChar:
    case general:
      reportError(loc,"invalid target for update");
      return Error;

    case rawInt:{
      Register reg = R0;
      AF2L(code,reg,fpReg);

      switch(v->where){
      case basedVar:
	AStI(code,v->base,v->l.off,reg);
	return Ok;
      case registr:
	AMove(code,v->l.reg,reg);
	return Ok;
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }

    case rawLong:{
      Register reg = R0;
      AF2L(code,reg,fpReg);

      switch(v->where){
      case basedVar:
	AStL(code,v->base,v->l.off,reg);
	return Ok;
      case registr:
	AMove(code,v->l.reg,reg);
	return Ok;
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }

    case rawFloat:{
      switch(v->where){
      case basedVar:
	AStD(code,v->base,v->l.off,fpReg);
	return Ok;
      case registr:
	AMoveDD(code,v->l.fpReg,fpReg);
	return Ok;
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    }
  }
  else{
    switch(v->kind){
    case rawChar:{
      switch(v->where){
      case basedVar:{
	if(src->where==registr)
	  AStC(code,v->base,v->l.off,src->l.reg);
	else{
	  Register reg = R0;
	  loadReg(loc,src,&reg,code);
	  
	  AStC(code,v->base,v->l.off,reg);
	}
	return Ok;
      }
      case registr:{
	Register reg = v->l.reg;
	return loadReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    case rawInt:{
      switch(v->where){
      case basedVar:{
	Register reg = R0;
	loadReg(loc,src,&reg,code);
	AStI(code,v->base,v->l.off,reg);
	return Ok;
      }
      case registr:{
	Register reg = v->l.reg;
	return loadReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    case rawLong:{
      switch(v->where){
      case basedVar:{
	Register reg = R0;
	loadReg(loc,src,&reg,code);
	AStL(code,v->base,v->l.off,reg);
	return Ok;
      }
      case registr:{
	Register reg = v->l.reg;
	return loadReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    case rawFloat:{
      switch(v->where){
      case basedVar:{
	FpRegister reg = FPR0;
	loadFpReg(loc,src,&reg,code);
	AStD(code,v->base,v->l.off,reg);
	return Ok;
      }
      case registr:{
	FpRegister reg = v->l.fpReg;
	return loadFpReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }

    case general:{
      switch(v->where){
      case basedVar:{
	if(src->where==registr)
	  ASt(code,v->base,v->l.off,src->l.reg);
	else{
	  Register reg = R0;
	  loadReg(loc,src,&reg,code);
	  ASt(code,v->base,v->l.off,reg);
	}
	return Ok;
      }
      case registr:{
	Register reg = v->l.reg;
	return loadReg(loc,src,&reg,code);
      }
      default:
	reportError(loc,"invalid target for assign");
	return Error;
      }
    }
    default:
      reportError(loc,"invalid target for variable");
      return Error;
    }
  }
  return Error;
}


retCode copyToT(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  int off = *(int*)cl;
  VarInfoRec tgt = {.loc=loc,.where=basedVar,.base=TRM,.kind=src->kind,.l.off=off};

  return assignVar(loc,src,&tgt,code);
}

retCode comboCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  comboPo combo = (comboPo)cl;
  tryRet(combo->cont1(loc,src,combo->cl1,code));
  return combo->cont2(loc,src,combo->cl2,code);
}

retCode jumpCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  AJmp(code,(lPo)cl);
  return Ok;
}

retCode nullCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  return Ok;
}

retCode errorCont(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  AHalt(code,87);			/* Fix me: put in info about the location */
  return Ok;
}


long typeSize(sxPo type)
{
  switch(tpKind(type)){
  case TypeVar:
    return POINTER_SIZE;
  case TypeExp:
    if(isRawCharType(type))
      return CHAR_SIZE;
    else if(isRawIntType(type))
      return INTEGER_SIZE;
    else if(isRawLongType(type))
      return LONG_SIZE;
    else if(isRawFloatType(type))
      return DOUBLE_SIZE;
    else
      return POINTER_SIZE;
  case ArrowType:
    return POINTER_SIZE;
  default:
    return POINTER_SIZE;
  }
}

sourceKind typeRep(sxPo type)
{
  return isRawCharType(type)?rawChar:
    isRawIntType(type)?rawInt:
    isRawLongType(type)?rawLong:
    isRawFloatType(type)?rawFloat:
    general;
}

long sourceSize(sourceKind mcType)
{
  switch(mcType){
  case general:
    return POINTER_SIZE;
  case rawChar:
    return CHAR_SIZE;
  case rawInt:
    return INTEGER_SIZE;
  case rawLong:
    return LONG_SIZE;
  case rawFloat:
    return DOUBLE_SIZE;
  }
  return POINTER_SIZE;
}



