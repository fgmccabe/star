/*
 * Handle type casting
 * 
 * Essentially, there are two cases: either the required cast is a pure
 * arithmetic cast, in which case a specific instruction is
 * generated. Otherwise, we look for a function that can do the type conversion.
 */

#include "compiler.h"
#include "dict.h"
#include "compile.h"
#include "assem.h"

//For now, only deal with arithmetic

retCode typeConvert(locationPo loc,varInfoPo src,void *cl,assemPo code)
{
  varInfoPo tgt = (varInfoPo)cl;

  switch(tgt->kind){
  case rawChar:				/* We are converting to a char */
    switch(src->kind){
    case rawChar:
    case rawInt:
    case rawLong:			/* Nothing special for integer types */
      return assignVar(loc,src,tgt,code);
    case rawFloat:{
      switch(tgt->where){
      case registr:{
	FpRegister reg = FPR0;
	loadFpReg(loc,src,&reg,code);
	AF2L(code,tgt->l.reg,reg);
	return Ok;
      }
      case basedVar:{
	FpRegister reg = FPR0;
	loadFpReg(loc,src,&reg,code);
	AF2L(code,R0,reg);
	AStC(code,tgt->base,tgt->l.off,R0);
	return Ok;
      }
      default:
	reportError(loc,"cannot convert value at %s to #char",
		    sourceName(src->where));
	return Error;
      }
    }
    case general:
      reportError(loc,"cannot convert from %s to #char",kindName(src->kind));
      return Error;
    }
  case rawInt:				/* converting to an integer */
    switch(src->kind){
    case rawChar:
      switch(tgt->where){
      case registr:
	loadReg(loc,src,&tgt->l.reg,code);
	AC2I(code,tgt->l.reg);
	return Ok;
      case basedVar:{
	Register reg = R0;
	loadReg(loc,src,&reg,code);
	AC2I(code,reg);			/* convert char to integer */
	AStI(code,tgt->base,tgt->l.off,R0);
	return Ok;
      }
      default:
	reportError(loc,"cannot convert value at %s to #int",
		    sourceName(src->where));
	return Error;
      }
    case rawInt:
      return assignVar(loc,src,tgt,code);
    case rawLong:
      switch(tgt->where){
      case registr:
	loadReg(loc,src,&tgt->l.reg,code);
	return Ok;
      case basedVar:{
	Register reg = R0;
	loadReg(loc,src,&reg,code);
	AStI(code,tgt->base,tgt->l.off,reg);
	return Ok;
      }
      default:
	reportError(loc,"cannot convert value at %s to #int",
		    sourceName(src->where));
	return Error;
      }

    case rawFloat:{			/* converting a float to an integer */
      FpRegister d = FPR0;
      loadFpReg(loc,src,&d,code);
      switch(tgt->where){
      case registr:
	AF2L(code,tgt->l.reg,d);
	return Ok;
      case basedVar:
	AF2L(code,R0,d);
	AStI(code,tgt->base,tgt->l.off,R0);
	return Ok;
      default:
	reportError(loc,"cannot convert value at %s to #int",
		    sourceName(src->where));
	return Error;
      }
    }
    case general:
      reportError(loc,"cannot convert from %s to #int",kindName(src->kind));
      return Error;
    }

  case rawLong:				/* converting to a long */
    switch(src->kind){
    case rawChar:
      switch(tgt->where){
      case registr:
	loadReg(loc,src,&tgt->l.reg,code);
	AC2I(code,tgt->l.reg);
	AI2L(code,tgt->l.reg);		/* need two conversions */
	return Ok;
      case basedVar:{
	Register reg = R0;
	loadReg(loc,src,&reg,code);
	AC2I(code,reg);			/* convert char to integer */
	AI2L(code,reg);
	AStL(code,tgt->base,tgt->l.off,R0);
	return Ok;
      }
      default:
	reportError(loc,"cannot convert value at %s to #long",
		    sourceName(src->where));
	return Error;
      }
    case rawInt:			/* convert integer to a long */
      switch(tgt->where){
      case registr:
	loadReg(loc,src,&tgt->l.reg,code);
	AI2L(code,tgt->l.reg);
	return Ok;
      case basedVar:{
	Register reg = R0;
	loadReg(loc,src,&reg,code);
	AI2L(code,reg);
	AStL(code,tgt->base,tgt->l.off,reg);
	return Ok;
      }
      default:
	reportError(loc,"cannot convert value at %s to #int",
		    sourceName(src->where));
	return Error;
      }

    case rawLong:
      return assignVar(loc,src,tgt,code);

    case rawFloat:{			/* converting a float to a long */
      FpRegister d = FPR0;
      loadFpReg(loc,src,&d,code);
      switch(tgt->where){
      case registr:
	AF2L(code,tgt->l.reg,d);
	return Ok;
      case basedVar:
	AF2L(code,R0,d);
	AStL(code,tgt->base,tgt->l.off,R0);
	return Ok;
      default:
	reportError(loc,"cannot convert value at %s to #int",
		    sourceName(src->where));
	return Error;
      }
    }
    case general:
      reportError(loc,"cannot convert from %s to #long",kindName(src->kind));
      return Error;
    }

  case rawFloat:			/* converting to a floating point */
    switch(src->kind){
    case rawChar:{
      Register reg = R0;
      loadReg(loc,src,&reg,code);
      AC2I(code,reg);			/* first convert from char to int */
      AI2L(code,reg);			/* then to long */
      
      switch(tgt->where){		/* then to float */
      case registr:
	AL2F(code,tgt->l.fpReg,reg);
	return Ok;
      case basedVar:{
	FpRegister fp = FPR0;
	AL2F(code,fp,reg);
	AStD(code,tgt->base,tgt->l.off,fp);
	return Ok;
      }
      default:
	reportError(loc,"cannot convert from %s to #float",kindName(src->kind));
	return Error;
      }
    }
    case rawInt:{
      Register reg = R0;
      loadReg(loc,src,&reg,code);
      AI2L(code,reg);			/* convert to long first */
      
      switch(tgt->where){
      case registr:
	AL2F(code,tgt->l.reg,reg);
	return Ok;
      case basedVar:{
	FpRegister fp = FPR0;
	AL2F(code,fp,reg);
	AStD(code,tgt->base,tgt->l.off,fp);
	return Ok;
      }
      default:
	reportError(loc,"cannot convert from %s to #float",kindName(src->kind));
	return Error;
      }
    }
    case rawLong:{
      Register reg = R0;
      loadReg(loc,src,&reg,code);
      
      switch(tgt->where){
      case registr:
	AL2F(code,tgt->l.reg,reg);
	return Ok;
      case basedVar:{
	FpRegister fp = FPR0;
	AL2F(code,fp,reg);
	AStD(code,tgt->base,tgt->l.off,fp);
	return Ok;
      }
      default:
	reportError(loc,"cannot convert from %s to #float",kindName(src->kind));
	return Error;
      }
    }
    case rawFloat:
      return assignVar(loc,src,tgt,code);
    case general:
      reportError(loc,"cannot convert from %s to #float",kindName(src->kind));
      return Error;
    }
  default:
  case general:
    reportError(loc,"cannot convert from %s to %s",kindName(src->kind),
		kindName(tgt->kind));
    return Error;
  }
}

