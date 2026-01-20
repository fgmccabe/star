//
// Created by Francis McCabe on 1/14/26.
//

instr(Halt, lcl)
instr(Abort, lit lcl)

instr(Call, sym lcl lcls)
instr(OCall, art lcl lcl lcls)
instr(Escape, Es lcl lcls)
instr(XCall, sym lVl lcl lcls)
instr(XOCall, art lVl lcl lcl lcls)
instr(XEscape, Es lVl lcl lcls)
instr(TCall, sym lcls)
instr(TOCall, art lcl lcls)
instr(Entry, i32)

instr(Block, bLk)
instr(Valof, lcl bLk)
instr(Break, lVl)
instr(Result, lVl lcl)
instr(Loop, lVl)

instr(Fiber, lcl)
instr(Suspend, lcl lcl)
instr(Resume, lcl lcl)
instr(Retire, lcl lcl)
instr(Underflow, none)

instr(MvV, lcl)
instr(MvC, lcl lit)
instr(Mv, lcl lcl)

instr(MvG, lcl glb)
instr(StG, glb lcl)

instr(Sav, lcl)
instr(LdSav, lcl lVl lcl)
instr(TstSav, lcl lcl)
instr(StSav, lcl lcl)

instr(Cell, lcl lcl)
instr(Get, lcl lcl)
instr(Assign, lcl lcl)

instr(CLbl, sym lVl lcl)
instr(CInt, lit lVl lcl)
instr(CChar, lit lVl lcl)
instr(CFlt, lit lVl lcl)
instr(CLit, lit lVl lcl)
instr(Nth, lcl i32 lcl)
instr(StNth, lcl i32 lcl lcl)

instr(If, lVl lcl)
instr(IfNot, lVl lcl)

instr(ICase, lcl bLk)
instr(Case, lcl bLk)
instr(IxCase, lcl bLk)

instr(IAdd, lcl lcl lcl)
instr(ISub, lcl lcl lcl)
instr(IMul, lcl lcl lcl)
instr(IDiv, lVl lcl lcl lcl)
instr(IMod, lVl lcl lcl lcl)
instr(IAbs, lcl lcl)

instr(IEq, lcl lcl lcl)
instr(ILt, lcl lcl lcl)
instr(IGe, lcl lcl lcl)

instr(CEq, lcl lcl lcl)
instr(CLt, lcl lcl lcl)
instr(CGe, lcl lcl lcl)

instr(BAnd, lcl lcl lcl)
instr(BOr, lcl lcl lcl)
instr(BXor, lcl lcl lcl)
instr(BLsl, lcl lcl lcl)
instr(BLsr, lcl lcl lcl)
instr(BAsr, lcl lcl lcl)
instr(BNot, lcl lcl)

instr(FAdd, lcl lcl lcl)
instr(FSub, lcl lcl lcl)
instr(FMul, lcl lcl lcl)
instr(FDiv, lVl lcl lcl lcl)
instr(FMod, lVl lcl lcl lcl)
instr(FAbs, lcl lcl)

instr(FEq, lcl lcl lcl)
instr(FLt, lcl lcl lcl)
instr(FGe, lcl lcl lcl)

instr(Alloc, lcl sym lcls)
instr(Closure, lcl sym lcl)

instr(Line, lit)
instr(Bind, lit lcl)
instr(dBug, lit)
