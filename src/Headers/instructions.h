//
// Created by Francis McCabe on 1/14/26.
//

instr(Halt, lcl)
instr(Abort, lit lcl)

instr(Call, sym lcls)
instr(OCall, lcl lcls)
instr(Escape, Es lcls)
instr(TCall, sym lcls)
instr(TOCall, lcl lcls)
instr(RSP, oUt)
instr(RSX, lVl oUt)
instr(Entry, lcm lcm)
instr(Rtn, none)
instr(Ret, lcl)
instr(XRet, lcl)

instr(Loop, bLk)
instr(Block, pHis bLk)
instr(Break, lVl)
instr(Result, lVl lcls)
instr(Cont, lVl)

instr(ICase, lcl bLk)
instr(Case, lcl bLk)
instr(IxCase, lcl bLk)

instr(CLbl, sym lVl lcl)
instr(CInt, lit lVl lcl)
instr(CChar, lit lVl lcl)
instr(CFlt, lit lVl lcl)
instr(CLit, lit lVl lcl)

instr(MC, oUt lit)
instr(Mv, oUt lcl)

instr(LG, glb)
instr(SG, glb lcl)

instr(Sav, oUt)
instr(LdSav, oUt lVl lcl)
instr(TstSav, oUt lcl)
instr(StSav, oUt lcl lcl)

instr(Cell, oUt lcl)
instr(Get, oUt lcl)
instr(Assign, lcl lcl)

instr(Nth, oUt i32 lcl)
instr(StNth, lcl i32 lcl)

instr(IAdd, oUt lcl lcl)
instr(ISub, oUt lcl lcl)
instr(IMul, oUt lcl lcl)
instr(IDiv, lVl oUt lcl lcl)
instr(IMod, lVl oUt lcl lcl)
instr(IAbs, oUt lcl)

instr(IEq, oUt lcl lcl)
instr(ILt, oUt lcl lcl)
instr(IGe, oUt lcl lcl)

instr(CEq, oUt lcl lcl)
instr(CLt, oUt lcl lcl)
instr(CGe, oUt lcl lcl)

instr(BAnd, oUt lcl lcl)
instr(BOr, oUt lcl lcl)
instr(BXor, oUt lcl lcl)
instr(BLsl, oUt lcl lcl)
instr(BLsr, oUt lcl lcl)
instr(BAsr, oUt lcl lcl)
instr(BNot, oUt lcl)

instr(FAdd, oUt lcl lcl)
instr(FSub, oUt lcl lcl)
instr(FMul, oUt lcl lcl)
instr(FDiv, lVl oUt lcl lcl)
instr(FMod, lVl oUt lcl lcl)
instr(FAbs, oUt lcl)

instr(FEq, oUt lcl lcl)
instr(FLt, oUt lcl lcl)
instr(FGe, oUt lcl lcl)

instr(Alloc, sym oUt lcls)
instr(Closure, sym oUt lcl)
instr(Bump, lcl)
instr(Drop, lcl)

instr(Fiber, oUt lcl)
instr(Suspend, lcl lcl)
instr(Resume, lcl lcl)
instr(Retire, lcl lcl)
instr(Underflow, none)

instr(Line, lit)
instr(Bind, lit lcl)
instr(dBug, lit)
