//
// Created by Francis McCabe on 11/4/25.
//


#include "ssaP.h"

static retCode insertSplit(insPo code,int32 start,int32 pc, int32 tgt, int32 limit);

retCode splitBlock(insPo code, int32 start, int32 limit){
  for(int32 pc=start;pc<limit;pc++){
    intPo ins = &code[pc];

    switch(ins->op){
      case Halt: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: Halt should be last instruction in block", pc);

	return Ok;
      }
      case Abort: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: Abort should be last instruction in block", pc);

	return Ok;
      }
      case Call: {
        if (code[pc + 1].op != Frame)
          return reportError(&ctx, ".%d: expecting a frame instruction after call", pc);
        continue;
      }
      case XCall: {
        if (code[pc + 1].op != Frame)
          return reportError(&ctx, ".%d: expecting a frame instruction after call", pc);

        if (insertSplit(code, start, pc+2, pc + code[pc].alt + 1, limit) != Ok)
          return Error;
        continue;
      }

      case TCall: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: TCall should be last instruction in block", pc);

        return Ok;
      }
      case OCall: {
        if (code[pc + 1].op != Frame)
          return reportError(&ctx, ".%d: expecting a frame instruction after ocall", pc);

        continue;
      }
      case XOCall: {
        if (code[pc + 1].op != Frame)
          return reportError(&ctx, ".%d: expecting a frame instruction after xocall", pc);

        if (insertSplit(code, start, pc+2, pc + code[pc].alt + 1, limit) != Ok)
          return Error;

        continue;
      }
      case TOCall: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: TCall should be last instruction in block", pc);

        return Ok;
      }
      case Escape: {
	if (code[pc + 1].op != Frame)
	  return reportError(&ctx, ".%d: expecting a frame instruction after escape", pc);

	continue;
      }
      case XEscape: {
	if (code[pc + 1].op != Frame)
	  return reportError(&ctx, ".%d: expecting a frame instruction after escape", pc);
        if (insertSplit(code, start, pc+2, pc + code[pc].alt + 1, limit) != Ok)
          return Error;

	continue;
      }
      case Entry: {
        continue;
      }
      case Ret: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: Ret should be last instruction in block", pc - 1);

        return Ok;
      }
      case XRet: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: Ret should be last instruction in block", pc - 1);
        return Ok;
      }
      case Block: {
        int32 blockLen = code[pc].alt;

	if(insertSplit(code,start,pc+1, pc+blockLen+1, limit)!=Ok)
	  return Error;
	
	if(splitBlock(code,pc+1,pc+blockLen)!=Ok)
	  return Error;
	pc+=blockLen;
	continue;
      }
      case Valof: {
        int32 blockLen = code[pc].alt;

	if(insertSplit(code,start,pc+1, pc+blockLen+1, limit)!=Ok)
	  return Error;
	
	if(splitBlock(code,pc+1,pc+blockLen)!=Ok)
	  return Error;
	pc+=blockLen;
	continue;
      }
      case Loop: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: Loop should be last instruction in block", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1) != Ok)
          return Error;
        return Ok;
      }
      case Break: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: Break should be last instruction in block", pc);

        if (checkLoop(&ctx, pc, pc + code[pc].alt + 1) != Ok)
          return Error;
        return Ok;
      }
      case Result: {
        if (!isLastPC(pc, limit))
          return reportError(&ctx, ".%d: Result should be last instruction in block", pc);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1) != Ok)
          return Error;
        return Ok;
      }

      case Drop: {
        continue;
      }

      case Dup: {
        continue;
      }

      case Rot: {
        continue;
      }
      case Rst: {
        continue;
      }
      case Fiber: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient stack depth for Fiber", pc);
        pc++;
        continue;
      }

      case Resume:
      case Suspend: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient stack depth for Resume/Suspend", pc);
        pc++;
        stackDepth--;
        continue;
      }

      case Retire: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient stack depth for Retire", pc);
        if (!isLastPC(pc++, limit))
          return reportError(&ctx, ".%d: Retire should be last instruction in block", pc);

        propagateVars(&ctx, parentCtx);
        return Ok;
      }
      case Underflow:
        return reportError(&ctx, ".%d: special instruction illegal in regular code %", pc);
      case LdV:
        stackDepth++;
        pc++;
        continue;
      case LdC: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, constant);
        stackDepth++;
        pc++;
        continue;
      }
      case LdA: {
        int32 argNo = code[pc].fst;
        if (argNo < 0 || argNo >= mtdArity(ctx.mtd))
          return reportError(&ctx, ".%d Out of bounds argument number: %d", pc, argNo);
        stackDepth++;
        pc++;
        continue;
      }
      case LdL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= lclCount(ctx.mtd))
          return reportError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        locals[lclNo].read = True;
        if (!locals[lclNo].inited)
          return reportError(&ctx, ".%d Read from uninitialized local %d", pc, lclNo);
        stackDepth++;
        pc++;
        continue;
      }
      case StL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= ctx.lclCount)
          return reportError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        locals[lclNo].inited = True;
        stackDepth--;
        pc++;
        continue;
      }
      case StV:
      case TL: {
        int32 lclNo = code[pc].fst - 1;
        if (lclNo < 0 || lclNo >= ctx.lclCount)
          return reportError(&ctx, ".%d Out of bounds local number: %d", pc, lclNo);
        locals[lclNo].inited = True;
        pc++;
        continue;
      }
      case LdG: {
        int32 glbNo = code[pc].fst;

        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return reportError(&ctx, ".%d unknown global variable: %d", pc, glbNo);
        stackDepth++;
        pc++;
        continue;
      }
      case StG: {
        int32 glbNo = code[pc].fst;
        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return reportError(&ctx, ".%d unknown global variable: %d", pc, glbNo);
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case TG: {
        int32 glbNo = code[pc].fst;
        globalPo glb = findGlobalVar(glbNo);
        if (glb == Null)
          return reportError(&ctx, ".%d unknown global variable: %d", pc, glbNo);
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case Sav: {
        stackDepth++;
        pc++;
        continue;
      }
      case TstSav: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case LdSav: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);

        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0, False) != Ok)
          return Error;

        pc++;
        continue;
      }
      case StSav: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        pc++;
        continue;
      }
      case TSav: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case Cell: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case Get: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case Assign: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        pc++;
        continue;
      }
      case CLit: {
        int32 key = code[pc].fst;
        if (!isDefinedConstant(key))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, key);
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0, False) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CInt: {
        int32 key = code[pc].fst;
        termPo lit = getConstant(key);

        if (lit == Null || !isInteger(lit))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, key);
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0, False) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CFlt: {
        int32 key = code[pc].fst;
        termPo lit = getConstant(key);

        if (lit == Null || !isFloat(lit))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, key);
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0, False) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CChar: {
        int32 key = code[pc].fst;
        termPo lit = getConstant(key);

        if (lit == Null || !isChar(lit))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, key);
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0, False) != Ok)
          return Error;
        pc++;
        continue;
      }
      case CLbl: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return reportError(&ctx, ".%d: invalid label: %T", pc, lit);
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0, False) != Ok)
          return Error;
        pc++;
        continue;
      }
      case Nth: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case StNth: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth -= 2;
        pc++;
        continue;
      }
      case If:
      case IfNot: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient values on stack: %d", pc, stackDepth);
        stackDepth--;
        if (checkBreak(&ctx, pc, pc + code[pc].alt + 1, 0, False) != Ok)
          return Error;
        pc++;
        continue;
      }
      case ICase:
      case Case:
      case IxCase: {
        int32 mx = code[pc].fst;
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
        for (int32 ix = 0; ix < mx; ix++) {
          int32 casePc = pc + 1 + ix;
          insPo caseIns = &code[casePc];
          switch (caseIns->op) {
            case Break:
            case Loop:
              if (checkBreak(&ctx, casePc, casePc + code[casePc].alt + 1, 0, False) != Ok)
                return Error;
              continue;
            default:
              return reportError(&ctx, ".%d: invalid case instruction", casePc);
          }
        }
        if (!isLastPC(pc + mx + 1, limit))
          return reportError(&ctx, ".%d: Case should be last instruction in block", pc);

        propagateVars(&ctx, parentCtx);
        return Ok;
      }
      case IAdd:
      case ISub:
      case IMul: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case IDiv:
      case IMod: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case IAbs: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case IEq:
      case ILt:
      case IGe: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case CEq:
      case CLt:
      case CGe:
      case BAnd:
      case BOr:
      case BXor:
      case BLsl:
      case BLsr:
      case BAsr: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case BNot: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case FAdd:
      case FSub:
      case FMul: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case FDiv:
      case FMod: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth--;
        pc++;
        continue;
      }
      case FAbs: {
        if (stackDepth < 1)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        pc++;
        continue;
      }
      case FEq:
      case FLt:
      case FGe: {
        if (stackDepth < 2)
          return reportError(&ctx, ".%d: insufficient args on stack: %d", pc, stackDepth);
        stackDepth -= 1;
        pc++;
        continue;
      }
      case Alloc: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return reportError(&ctx, ".%d: invalid symbol literal: %t", pc, lit);
        else {
          int32 arity = lblArity(C_LBL(lit));
          if (stackDepth < arity)
            return reportError(&ctx, ".%d: insufficient stack args for Alloc instruction", pc);
          else if (code[pc + 1].op != Frame)
            return reportError(&ctx, ".%d: expecting Frame instruction after Alloc", pc);
          else {
            stackDepth = stackDepth - arity + 1;
            pc++;
            continue;
          }
        }
      }
      case Closure: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, constant);

        termPo lit = getConstant(constant);
        if (!isALabel(lit))
          return reportError(&ctx, ".%d: invalid Closure literal: %t", pc, lit);
        else {
          if (stackDepth < 1)
            return reportError(&ctx, ".%d: insufficient stack args for Closure instruction", pc);
          else {
            pc++;
            continue;
          }
        }
      }
      case Frame: {
        int32 depth = code[pc].fst;
        if (depth != stackDepth)
          return reportError(&ctx, ".%d: stack depth %d does not match Frame instruction %d", pc,
                             stackDepth, depth);
        pc++;
        continue;
      }
      case Line:
      case dBug: {
        int32 constant = code[pc].fst;
        if (!isDefinedConstant(constant))
          return reportError(&ctx, ".%d: invalid constant number: %d ", pc, constant);
        pc++;
        continue;
      }
      default:
        return reportError(&ctx, ".%d: illegal instruction", pc);
    }

  }
}

