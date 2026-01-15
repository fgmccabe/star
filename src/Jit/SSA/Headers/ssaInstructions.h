//
// Created by Francis McCabe on 1/14/26.
//

#ifndef STAR_SSAINS_H
#define STAR_SSAINS_H

instr(Halt, 1, lcl)
instr(Abort, 2, lit, lcl)

instr(Call, 2, sym, lcls)
instr(OCall, 3, art, lcl, lcls)
instr(Escape, 2, Es, lcls)
instr(XCall, 3, sym, lVl, lcls)
instr(XOCall, 4, art, lVl, lcl, lcls)
instr(XEscape, 3, Es, lVl, lcls)
instr(TCall, 2, sym, lcls)
instr(TOCall, 3, art, lcl, lcls)
instr(Entry, 1, i32)

instr(Ret, 1, lcl)
instr(XRet, 1, lcl)

instr(Block, 2, i32, bLk)
instr(Valof, 3, i32, bLk, lcl)
instr(Break, 1, lVl)
instr(Result, 2, lVl, lcl)
instr(Loop, 1, lVl)

instr(Fiber, 1, lcl)
instr(Suspend, 2, lcl, lcl)
instr(Resume, 2, lcl, lcl)
instr(Retire, 2, lcl, lcl)
instr(Underflow, 0)

instr(MvV, 1,lcl)
instr(MvC, 2, lcl, lit)
instr(Mv, 2, lcl, lcl)

instr(MvG, 2, lcl, glb)
instr(StG, 2, glb, lcl)

instr(Sav, 1, lcl)
instr(LdSav, 3, lcl, lVl, lcl)
instr(TstSav, 2, lcl, lclt)
instr(StSav, 2, lcl, lcl)

instr(Cell, 1, lcl)
instr(Get, 2, lcl, lcl)
instr(Assign, 2, lcl, lcl)

instr(CLbl, 3, sym, lVl, lcl)
instr(CInt, 3, lit, lVl, lcl)
instr(CChar, 3, lit, lVl, lcl)
instr(CFlt, 3, lit, lVl, lcl)
instr(CLit, 3, lit, lVl, lcl)
instr(Nth, 3, lcl, i32, lcl)
instr(StNth, 3, lcl, i32, lcl)

instr(If, 2, lVl,lcl)
instr(IfNot, 2, lVl, lcl)

instr(ICase, 2, lcl, lVls)
instr(Case, 2, lcl, lVls)
instr(IxCase, 2, lcl, lVls)

instr(IAdd, 3, lcl, lcl, lcl)
instr(ISub, 3, lcl, lcl, lcl)
instr(IMul, 3, lcl, lcl, lcl)
instr(IDiv, 3, lcl, lcl, lcl)
instr(IMod, 3, lcl, lcl, lcl)
instr(IAbs, 2, lcl, lcl)

instr(IEq, 3, lcl, lcl, lcl)
instr(ILt, 3, lcl, lcl, lcl)
instr(IGe, 3, lcl, lcl, lcl)

instr(CEq, 3, lcl, lcl, lcl)
instr(CLt, 3, lcl, lcl, lcl)
instr(CGe, 3, lcl, lcl, lcl)

instr(BAnd, 3, lcl, lcl, lcl)
instr(BOr, 3, lcl, lcl, lcl)
instr(BXor, 3, lcl, lcl, lcl)
instr(BLsl, 3, lcl, lcl, lcl)
instr(BLsr, 3, lcl, lcl, lcl)
instr(BAsr, 3, lcl, lcl, lcl)
instr(BNot, 2, lcl, lcl)

instr(FAdd, 3, lcl, lcl, lcl)
instr(FSub, 3, lcl, lcl, lcl)
instr(FMul, 3, lcl, lcl, lcl)
instr(FDiv, 3, lcl, lcl, lcl)
instr(FMod, 3, lcl, lcl, lcl)
instr(FAbs, 2, lcl, lcl)

instr(FEq, 3, lcl, lcl, lcl)
instr(FLt, 3, lcl, lcl, lcl)
instr(FGe, 3, lcl, lcl, lcl)

instr(Alloc, 3, lcl, sym,lcls)
instr(Closure, 3, lcl, sym, lcl)

instr(Line, 1, lit)
instr(Bind, 2, lit, lcl)
instr(dBug, 1, lit)

#endif //STAR_SSAINS_H
