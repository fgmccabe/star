/*
  Specification of the Star Bytecode Instruction set
  (c) 2017, and beyond F.G.McCabe

  This file should NEVER be included directly. It must always be
  preceded and followed by undefs. A typical sequence looks like:

#undef instruction
#define instruction(Op,A1,A2,Dl,Cmnt) ...<expansion>...

#include "instruction.h"
#undef instruction

  Contact: Francis McCabe <fmccabe@gmail.com>
*/

instruction(Halt, i32, nOp, 0, "F()v", "Stop execution")
instruction(Nop, nOp, nOp, 0, "F()()", "No operation")

instruction(Abort, tOs, tOs, 0, "F(pp)v", "abort with message")

instruction(Call, sym, nOp, 1, "F()p", "Call <prog>")
instruction(OCall, art, tOs, 1, "F(p)p", "OCall")
instruction(Escape, Es, nOp, 1, "F()p", "call C escape")
instruction(XCall, sym, lVl, 1, "F()p", "Call <prog>")
instruction(XOCall, art, lVl, 1, "F(p)p", "OCall")
instruction(XEscape, Es, lVl, 1, "F()p", "call C escape")
instruction(TCall, sym, nOp, 0, "F()v", "TCall <prog>")
instruction(TOCall, art, tOs, 0, "F()v", "TOCall")
instruction(Entry, i32, nOp, 0, "F()()", "locals definition")

instruction(Ret, tOs, nOp, 0, "F()v", "return")
instruction(XRet, tOs, nOp, 0, "F()v", "return")

instruction(Block, i32, bLk, 0, "F()()", "block of instructions")
instruction(Break, nOp, lVl, 0, "F()()", "leave block")
instruction(Result, i32, lVl, -1, "F(p)p", "return value out of block")
instruction(Loop, nOp, lVl, 0, "F()()", "jump back to start of block")

instruction(Drop, tOs, nOp, -1, "F(p)()", "drop top of stack")
instruction(Dup, tOs, nOp, 1, "F(p)(pp)", "duplicate top of stack")
instruction(Rot, i32, nOp, 0, "F()()", "Pull up nth element of stack")
instruction(Rst, i32, nOp, 0, "F()()", "reset stack height to a fixed height")
instruction(Pick, i32, i32, 0, "", "adjust stack to n depth, using top k elements")

instruction(Fiber, tOs, nOp, 0, "F(p)p", "Create new fiber")
instruction(Suspend, tOs, tOs, -1, "F(pp)()", "suspend fiber")
instruction(Resume, tOs, tOs, -1, "F(pp)()", "resume fiber")
instruction(Retire, tOs, tOs, -2, "F(pp)v", "retire a fiber")
instruction(Underflow, nOp, nOp, 0, "F()()", "underflow from current stack")

instruction(Try, i32, bLk, 1, "F()p", "a try-catch block")
instruction(EndTry, tOs, lVl, -1, "F(p)()", "end try")
instruction(TryRslt, tOs, lVl, -1, "F(pp)p", "end try with a  result")
instruction(Throw, tOs, tOs, 0, "F(p)v", "Invoke a continuation")

instruction(LdV, nOp, nOp, 1, "F()p", "Place a void value on stack")
instruction(LdC, lit, nOp, 1, "F()p", "load literal from constant pool")
instruction(LdA, arg, nOp, 1, "F()p", "load stack from args[xx]")
instruction(LdL, lcl, nOp, 1, "F()p", "load stack from local[xx]")
instruction(StL, lcs, tOs, -1, "F(p)()", "store tos to local[xx]")
instruction(StV, lcs, nOp, 0, "F()()", "clear a local to void")
instruction(TL, lcs, tOs, 0, "F(p)p", "copy tos to local[xx]")

instruction(LdG, glb, nOp, 1, "F()p", "load a global variable")
instruction(StG, glb, tOs, -1, "F(p)()", "store into a global variable")
instruction(TG, glb, tOs, 0, "F(p)p", "copy into a global variable")

instruction(Sav, nOp, nOp, 0, "F()p", "create a single assignment variable")
instruction(LdSav, tOs, lVl, 0, "F(p)p", "derefence a sav, break if not set")
instruction(TstSav, tOs, nOp, 0, "F(p)l", "test a sav, return a logical")
instruction(StSav, tOs, tOs, -2, "F(pp)()", "store a value into a single assignment variable")
instruction(TSav, tOs, tOs, -1, "F(pp)p", "update single assignment variable leave value on stack")

instruction(Cell, tOs, nOp, 0,"F()p",  "create R/W cell")
instruction(Get, tOs, nOp, 0, "F(p)p", "access a R/W cell")
instruction(Assign, tOs, tOs, -2, "F(pp)()", "assign to a R/W cell")

instruction(CLbl, sym, lVl, -1, "F()()", "T,Lbl --> test for a data term, break if not lbl")
instruction(CLit, lit, lVl, -1, "F(p)(p)", "T,lit --> test for a literal value, break if not")
instruction(Nth, i32, nOp, 0, "F(p)p", "T --> el, pick up the nth element")
instruction(StNth, i32, nOp, -2, "F(pp)()", "T el --> store in nth element")

instruction(If, tOs, lVl, -1,"F(p)()",  "break if true")
instruction(IfNot, tOs, lVl, -1,"F(p)()",  "break if false")

instruction(Case, i32, tOs, -1, "F(p)p", "T --> T, case <Max>")
instruction(IndxJmp, i32, tOs, -1, "F(p)p", "check and jump on index")

instruction(IAdd, tOs, tOs, -1, "F(ii)i", "L R --> L+R")
instruction(ISub, tOs, tOs, -1, "F(ii)i", "L R --> L-R")
instruction(IMul, tOs, tOs, -1, "F(ii)i", "L R --> L*R")
instruction(IDiv, tOs, lVl, -2, "F(ii)i", "L R --> L/R")
instruction(IMod, tOs, tOs, -2, "F(ii)i", "L R --> L%R")
instruction(IAbs, tOs, nOp, 0, "F(i)i", "L --> abs(L)")

instruction(IEq, tOs, tOs, -1, "F(ii)l", "L R --> L==R")
instruction(ILt, tOs, tOs, -1, "F(ii)l", "L R --> L<R")
instruction(IGe, tOs, tOs, -1, "F(ii)l", "L R --> L>=R")
instruction(ICmp, tOs, lVl, -2, "F(ii)()", "L R --> break if not same integer")

instruction(CEq, tOs, tOs, -1, "F(ii)l", "L R --> L==R")
instruction(CLt, tOs, tOs, -1, "F(ii)l", "L R --> L<R")
instruction(CGe, tOs, tOs, -1, "F(ii)l", "L R --> L>=R")
instruction(CCmp, tOs, lVl, -2, "F(ii)l", "L R --> break if not same character")

instruction(BAnd, tOs, tOs, -1, "F(ii)i", "L R --> L&R")
instruction(BOr, tOs, tOs, -1, "F(ii)i", "L R --> L|R")
instruction(BXor, tOs, tOs, -1, "F(ii)i", "L R --> L^R")
instruction(BLsl, tOs, tOs, -1, "F(ii)i", "L R --> L<<R")
instruction(BLsr, tOs, tOs, -1, "F(ii)i", "L R --> L>>R")
instruction(BAsr, tOs, tOs, -1, "F(ii)i", "L R --> L>>>R")
instruction(BNot, tOs, nOp, 0, "F(i)i", "L --> ~L")

instruction(FAdd, tOs, tOs, -1, "F(ff)f", "L R --> L+R")
instruction(FSub, tOs, tOs, -1, "F(ff)f", "L R --> L-R")
instruction(FMul, tOs, tOs, -1, "F(ff)f", "L R --> L*R")
instruction(FDiv, tOs, tOs, -1, "F(ff)f", "L R --> L/R")
instruction(FMod, tOs, tOs, -1, "F(ff)f", "L R --> L%R")
instruction(FAbs, tOs, nOp, 0, "F(f)f", "L --> abs(L)")

instruction(FEq, tOs, tOs, -1, "F(ff)l", "L R e --> L==R")
instruction(FLt, tOs, tOs, -1, "F(ff)l", "L R --> L<R")
instruction(FGe, tOs, tOs, -1, "F(ff)l", "L R --> L>=R")
instruction(FCmp, tOs, lVl, -2, "F(ff)()", "L R --> branch if not same floating point")

instruction(Alloc, sym, nOp, 1, "F()()", "new structure, elements from stack")
instruction(Closure, sym, tOs, 0, "F(p)p", "allocate a closure")

instruction(Cmp, tOs, lVl, -2, "F(pp)()", "t1 t2 --> , branch to offset if not same literal")

instruction(Frame, tPe, nOp, 0, "F()()", "frame instruction")

instruction(dBug, nOp, nOp, 0, "F()()", "debugging prefix")
