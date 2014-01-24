/*
 * Handle compilation of conditions
 */

#include "compiler.h"
#include "dict.h"
#include "utils.h"
#include "compile.h"
#include "codegen.h"

retCode compileCondition(sxPo cond,uniChar *path,
			 dictPo dict,dictPo outer,
			 jumpMode sense,lPo tgt,
			 exitPo exit,mtdPo mtd)
{
  assemPo code = methodCode(mtd);
  locationPo loc = sxLoc(cond);

  if(sxIsCondition(cond)){
    sxPo lhs = sxLhs(cond);
    sxPo rhs = sxRhs(cond);

    uniChar *pred = sxConditionOp(cond);

    sourceKind lMode = expMode(lhs,dict);
    sourceKind rMode = expMode(rhs,dict);

    if(lMode!=rMode){
      reportError(loc,"inconsistent expressions in condition: %A",cond);
      return Error;
    }

    if(lMode!=rawFloat){
      sxPo expType = Null;
      VarInfoRec lVar = {.loc=sxLoc(lhs),.where=registr,.kind=lMode,.l.reg=R1};
      VarInfoRec rVar = {.loc=sxLoc(rhs),.where=registr,.kind=lMode,.l.reg=R0};

      tryRet(compileExp(lhs,&expType,path,dict,outer,
			exit,mtd,loadReg,&lVar.l.reg));
      tryRet(compileExp(rhs,&expType,path,dict,outer,
			exit,mtd,loadReg,&rVar.l.reg));

      if(pred==EqualName){
	if(sense==onSuccess){
	  switch(lMode){
	  case rawChar:
	    ABeqC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABeqI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABeqL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABeq(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
	else{
	  switch(lMode){
	  case rawChar:
	    ABneC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABneI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABneL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABne(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
      }
      else if(pred==NotEqualName){
	if(sense==onFailure){
	  switch(lMode){
	  case rawChar:
	    ABeqC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABeqI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABeqL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABeq(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
	else{
	  switch(lMode){
	  case rawChar:
	    ABneC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABneI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABneL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABne(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
      }
      else if(pred==LessName){
	if(sense==onFailure){
	  switch(lMode){
	  case rawChar:
	    ABgeC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABgeI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABgeL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABge(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
	else{
	  switch(lMode){
	  case rawChar:
	    ABltC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABltI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABltL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABlt(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
      }
      else if(pred==LessEqualName){
	if(sense==onFailure){
	  switch(lMode){
	  case rawChar:
	    ABgtC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABgtI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABgtL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABgt(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
	else{
	  switch(lMode){
	  case rawChar:
	    ABleC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABleI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABleL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABle(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
      }
      else if(pred==GreaterName){
	if(sense==onSuccess){
	  switch(lMode){
	  case rawChar:
	    ABgtC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABgtI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABgtL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABgt(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
	else{
	  switch(lMode){
	  case rawChar:
	    ABleC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABleI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABleL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABle(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
      }
      else if(pred==GreaterEqualName){
	if(sense==onSuccess){
	  switch(lMode){
	  case rawChar:
	    ABgeC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABgeI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABgeL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABge(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
	else{
	  switch(lMode){
	  case rawChar:
	    ABltC(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawInt:
	    ABltI(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case rawLong:
	    ABltL(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  case general:
	    ABlt(code,lVar.l.reg,rVar.l.reg,tgt);
	    return Ok;
	  default:
	    reportError(loc,"invalid comparison expression %A",cond);
	    return Error;
	  }
	}
      }
      else{
	reportError(loc,"invalid condition: %A",cond);
	return Error;
      }
    }
    else{
      VarInfoRec lVar = {.loc=sxLoc(lhs),.where=registr,
			 .kind=rawFloat,.l.reg=FPR1};
      VarInfoRec rVar = {.loc=sxLoc(rhs),.where=registr,
			 .kind=rawFloat,.l.reg=FPR0};

      tryRet(compileExp(lhs,&floatType,path,dict,outer,
			exit,mtd,loadFpReg,&lVar));
      tryRet(compileExp(rhs,&floatType,path,dict,outer,
			exit,mtd,loadFpReg,&rVar));

      if(pred==EqualName){
	if(sense==onSuccess)
	  ABeqD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	else
	  ABneD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	return Ok;
      }
      else if(pred==NotEqualName){
	if(sense!=onSuccess)
	  ABeqD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	else
	  ABneD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	return Ok;
      }
      else if(pred==LessName){
	if(sense==onSuccess)
	  ABltD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	else
	  ABgeD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	return Ok;
      }
      else if(pred==LessEqualName){
	if(sense!=onSuccess)
	  ABltD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	else
	  ABgeD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	return Ok;
      }
      else if(pred==GreaterName){
	if(sense==onSuccess)
	  ABgtD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	else
	  ABleD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	return Ok;
      }
      else if(pred==GreaterEqualName){
	if(sense!=onSuccess)
	  ABgtD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	else
	  ABleD(code,lVar.l.fpReg,rVar.l.fpReg,tgt);
	return Ok;
      }
      else{
	reportError(loc,"invalid condition: %A",cond);
	return Error;
      }
    }
  }
  else{
    Register reg = R0;
    retCode ret = compileExp(cond,&booleanType,
			     path,dict,outer,exit,mtd,loadReg,&reg);
    if(ret==Ok){
      AMoveI(code,R1,0);
      if(sense==onSuccess)
	ABne(code,R0,R1,tgt);
      else
	ABeq(code,R0,R1,tgt);
    }
    return ret;
  }
}

