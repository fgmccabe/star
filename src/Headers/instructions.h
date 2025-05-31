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

instruction(Halt, i32, nOp, 0, "Stop execution")
instruction(Nop, nOp, nOp, 0, "No operation")

instruction(Abort, tOs, tOs, 0, "abort with message")

instruction(Call, sym, nOp, 1, "Call <prog>")
instruction(OCall, art, tOs, 1, "OCall")
instruction(Escape, Es, nOp, 1, "call C escape")
instruction(XCall, sym, lVl, 1, "Call <prog>")
instruction(XOCall, art, lVl, 1, "OCall")
instruction(XEscape, Es, lVl, 1, "call C escape")
instruction(TCall, sym, nOp, 0, "TCall <prog>")
instruction(TOCall, art, tOs, 0, "TOCall")
instruction(Entry, i32, nOp, 0, "locals definition")

instruction(Ret, tOs, nOp, 0, "return")
instruction(XRet, tOs, nOp, 0, "return exception")

instruction(Block, i32, bLk, 0, "block of instructions")
instruction(Break, nOp, lVl, 0, "leave block")
instruction(Result, nOp, lVl, -1, "return value out of block")
instruction(Loop, nOp, lVl, 0, "jump back to start of block")

instruction(Drop, tOs, nOp, -1, "drop top of stack")
instruction(Dup, tOs, nOp, 1, "duplicate top of stack")
instruction(Rot, i32, nOp, 0, "Pull up nth element of stack")
instruction(Rst, i32, nOp, 0, "reset stack height to a fixed height")
instruction(Pick, i32, i32, 0, "adjust stack to n depth, using top k elements")

instruction(Fiber, tOs, nOp, 0, "Create new fiber")
instruction(Suspend, tOs, tOs, -1, "suspend fiber")
instruction(Resume, tOs, tOs, -1, "resume fiber")
instruction(Retire, tOs, tOs, -2, "retire a fiber")
instruction(Underflow, nOp, nOp, 0, "underflow from current stack")

instruction(LdV, nOp, nOp, 1, "Place a void value on stack")
instruction(LdC, lit, nOp, 1, "load literal from constant pool")
instruction(LdA, arg, nOp, 1, "load stack from args[xx]")
instruction(LdL, lcl, nOp, 1, "load stack from local[xx]")
instruction(StL, lcs, tOs, -1, "store tos to local[xx]")
instruction(StV, lcs, nOp, 0, "clear a local to void")
instruction(TL, lcs, tOs, 0, "copy tos to local[xx]")

instruction(LdG, glb, nOp, 1, "load a global variable")
instruction(StG, glb, tOs, -1, "store into a global variable")
instruction(TG, glb, tOs, 0, "copy into a global variable")

instruction(Sav, nOp, nOp, 0, "create a single assignment variable")
instruction(LdSav, tOs, lVl, 0, "derefence a sav, break if not set")
instruction(TstSav, tOs, nOp, 0, "test a sav, return a logical")
instruction(StSav, tOs, tOs, -2, "store a value into a single assignment variable")
instruction(TSav, tOs, tOs, -1, "update single assignment variable leave value on stack")

instruction(Cell, tOs, nOp, 0, "create R/W cell")
instruction(Get, tOs, nOp, 0, "access a R/W cell")
instruction(Assign, tOs, tOs, -2, "assign to a R/W cell")

instruction(CLbl, sym, lVl, -1, "T,Lbl --> test for a data term, break if not lbl")
instruction(CInt, lit, lVl, -1, "T,lit --> test for a literal integer, break if not")
instruction(CChar, lit, lVl, -1, "T,lit --> test for a literal char, break if not")
instruction(CFlt, lit, lVl, -1, "T,lit --> test for a literal floating point, break if not")
instruction(CLit, lit, lVl, -1, "T,lit --> test for a literal value, break if not")
instruction(Nth, i32, nOp, 0, "T --> el, pick up the nth element")
instruction(StNth, i32, nOp, -2, "T el --> store in nth element")

instruction(If, tOs, lVl, -1, "break if true")
instruction(IfNot, tOs, lVl, -1, "break if false")

instruction(ICase, i32, tOs, -1, "T --> T, icase <Max>")
instruction(Case, i32, tOs, -1, "T --> T, case <Max>")
instruction(Unpack, i32, tOs, -1, "check and jump on index")

instruction(IAdd, tOs, tOs, -1, "L R --> L+R")
instruction(ISub, tOs, tOs, -1, "L R --> L-R")
instruction(IMul, tOs, tOs, -1, "L R --> L*R")
instruction(IDiv, tOs, lVl, -2, "L R --> L/R")
instruction(IMod, tOs, lVl, -2, "L R --> L%R")
instruction(IAbs, tOs, nOp, 0, "L --> abs(L)")

instruction(IEq, tOs, tOs, -1, "L R --> L==R")
instruction(ILt, tOs, tOs, -1, "L R --> L<R")
instruction(IGe, tOs, tOs, -1, "L R --> L>=R")
instruction(ICmp, tOs, lVl, -2, "L R --> break if not same integer")

instruction(CEq, tOs, tOs, -1, "L R --> L==R")
instruction(CLt, tOs, tOs, -1, "L R --> L<R")
instruction(CGe, tOs, tOs, -1, "L R --> L>=R")
instruction(CCmp, tOs, lVl, -2, "L R --> break if not same character")

instruction(BAnd, tOs, tOs, -1, "L R --> L&R")
instruction(BOr, tOs, tOs, -1, "L R --> L|R")
instruction(BXor, tOs, tOs, -1, "L R --> L^R")
instruction(BLsl, tOs, tOs, -1, "L R --> L<<R")
instruction(BLsr, tOs, tOs, -1, "L R --> L>>R")
instruction(BAsr, tOs, tOs, -1, "L R --> L>>>R")
instruction(BNot, tOs, nOp, 0, "L --> ~L")

instruction(FAdd, tOs, tOs, -1, "L R --> L+R")
instruction(FSub, tOs, tOs, -1, "L R --> L-R")
instruction(FMul, tOs, tOs, -1, "L R --> L*R")
instruction(FDiv, tOs, lVl, -1, "L R --> L/R")
instruction(FMod, tOs, lVl, -1, "L R --> L%R")
instruction(FAbs, tOs, nOp, 0, "L --> abs(L)")

instruction(FEq, tOs, tOs, -1, "L R e --> L==R")
instruction(FLt, tOs, tOs, -1, "L R --> L<R")
instruction(FGe, tOs, tOs, -1, "L R --> L>=R")
instruction(FCmp, tOs, lVl, -2, "L R --> branch if not same floating point")

instruction(Alloc, sym, nOp, 1, "new structure, elements from stack")
instruction(Closure, sym, tOs, 0, "allocate a closure")

instruction(Cmp, tOs, lVl, -2, "t1 t2 --> , branch to offset if not same literal")

instruction(Frame, i32, nOp, 0, "frame instruction")

instruction(dBug, nOp, nOp, 0, "debugging prefix")
