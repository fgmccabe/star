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

instruction(Abort, tOs, tOs, -2, "F(pp)v", "abort with message")

instruction(Call, sym, nOp, 1, "F()p", "Call <prog>")
instruction(OCall, art, nOp, 1, "F(p)p", "OCall")
instruction(Escape, Es, nOp, 1, "F()p", "call C escape")
instruction(TCall, sym, nOp, 0, "F()()", "TCall <prog>")
instruction(TOCall, art, tOs, 0, "F()()", "TOCall")
instruction(Locals, i32, nOp, 0, "F()()", "locals definition")

instruction(Ret, tOs, nOp, 0, "F()v", "return")

instruction(Block, tPe, bLk, 0, "F()()", "block of instructions")
instruction(Break, lVl, nOp, 0, "F()()", "leave block, or jump back to start of loop")

instruction(Drop, tOs, nOp, -1, "F(p)()", "drop top of stack")
instruction(Dup, tOs, nOp, 1, "F(p)(pp)", "duplicate top of stack")
instruction(Rot, i32, nOp, 0, "F()()", "Pull up nth element of stack")
instruction(Rst, i32, nOp, 0, "F()()", "reset stack height to a fixed height")

instruction(Fiber, tOs, nOp, 0, "F(p)p", "Create new fiber")
instruction(Spawn, tOs, nOp, 0, "F(p)p", "spawn a new task")
instruction(Suspend, tOs, tOs, -1, "F(pp)()", "suspend fiber")
instruction(Resume, tOs, tOs, -1, "F(pp)()", "resume fiber")
instruction(Retire, tOs, tOs, -2, "F(pp)v", "retire a fiber")
instruction(Underflow, nOp, nOp, 0, "F()()", "underflow from current stack")
instruction(TEq, tOs, tOs, -1, "F(pp)l", "L R --> L==R, where L,R are tasks")

instruction(Try, tPe, bLk, 1, "F()p", "a try-catch block")
instruction(EndTry, tOs, lVl, -1, "F(p)()", "end try block")
instruction(Throw, tOs, tOs, 0, "F(p)v", "Invoke a continuation")
instruction(Reset,tOs,nOp,0, "F(p)()", "establish a delimited zone")
instruction(Shift,tOs,tOs,-1,"F(pp)v", "capture continuation")

instruction(Invoke, tOs, tOs, -1, "F(pp)v", "invoke continuation")

instruction(LdV, nOp, nOp, 1, "F()p", "Place a void value on stack")
instruction(LdC, lit, nOp, 1, "F()p", "load literal from constant pool")
instruction(LdA, arg, nOp, 1, "F()p", "load stack from args[xx]")
instruction(LdL, lcl, nOp, 1, "F()p", "load stack from local[xx]")
instruction(StL, lcs, tOs, -1, "F(p)()", "store tos to local[xx]")
instruction(StV, lcs, nOp, 0, "F()()", "clear a local to void")
instruction(TL, lcs, tOs, 0, "F(p)p", "copy tos to local[xx]")
instruction(StA, arg, tOs, -1, "F(p)()", "store tos to args[xx]")

instruction(LdG, glb, nOp, 1, "F()p", "load a global variable")
instruction(StG, glb, tOs, -1, "F(p)()", "store into a global variable")
instruction(TG, glb, tOs, 0, "F(p)p", "copy into a global variable")

instruction(Thunk, tOs, nOp, 0, "F(p)p", "create a thunk from a lambda")
instruction(LdTh, tOs, nOp, 0, "F(p)p", "derefence a thunk, potentially running its lambda")
instruction(StTh, tOs, tOs, -2, "F(pp)()", "store a value into a thunk variable")
instruction(TTh, tOs, tOs, -1, "F(pp)p", "update thunk and leave on stack")

instruction(Cell, tOs, nOp, 0,"F()p",  "create R/W cell")
instruction(Get, tOs, nOp, 0, "F(p)p", "access a R/W cell")
instruction(Assign, tOs, tOs, -2, "F(pp)()", "assign to a R/W cell")

instruction(CLbl, sym, lVl, 0, "F()()", "T,Lbl --> test for a data term, break if lbl")
instruction(Nth, i32, nOp, 0, "F(p)p", "T --> el, pick up the nth element")
instruction(StNth, i32, nOp, -2, "F(pp)()", "T el --> store in nth element")

instruction(If, lVl, tOs, -1,"F(p)()",  "break if true")
instruction(IfNot, lVl, tOs, -1,"F(p)()",  "break if false")

instruction(Case, i32, tOs, 0, "F(p)p", "T --> T, case <Max> ")
instruction(IndxJmp, i32, tOs, 0, "F(p)p", "check and jump on index")
instruction(Unpack, sym, lVl, -1,"F(p)()", "check against term & unpack")

instruction(IAdd, tOs, tOs, -1, "F(ii)i", "L R --> L+R")
instruction(ISub, tOs, tOs, -1, "F(ii)i", "L R --> L-R")
instruction(IMul, tOs, tOs, -1, "F(ii)i", "L R --> L*R")
instruction(IDiv, tOs, tOs, -2, "F(ii)i", "L R --> L/R")
instruction(IMod, tOs, tOs, -2, "F(ii)i", "L R --> L%R")
instruction(IAbs, tOs, nOp, 0, "F(i)i", "L --> abs(L)")

instruction(IEq, tOs, tOs, -1, "F(ii)l", "L R --> L==R")
instruction(ILt, tOs, tOs, -1, "F(ii)l", "L R --> L<R")
instruction(IGe, tOs, tOs, -1, "F(ii)l", "L R --> L>=R")
instruction(ICmp, lVl, tOs, -2, "F(ii)()", "L R --> break if not same integer")

instruction(CEq, tOs, tOs, -1, "F(ii)l", "L R --> L==R")
instruction(CLt, tOs, tOs, -1, "F(ii)l", "L R --> L<R")
instruction(CGe, tOs, tOs, -1, "F(ii)l", "L R --> L>=R")
instruction(CCmp, lVl, tOs, -2, "F(ii)l", "L R --> break if not same character")

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
instruction(FGe, tOs, tOs, 1, "F(ff)l", "L R --> L>=R")
instruction(FCmp, lVl, tOs, -2, "F(ff)()", "L R --> branch if not same floating point")

instruction(Alloc, sym, nOp, 1, "F()()", "new structure, elements from stack")
instruction(Closure, sym, tOs, 0, "F(p)p", "allocate a closure")

instruction(Cmp, lVl, tOs, -2, "F(pp)()", "t1 t2 --> , branch to offset if not same literal")

instruction(Frame, tPe, nOp, 0, "F()()", "frame instruction")

instruction(dBug, nOp, nOp, 0, "F()()", "debugging prefix")
instruction(Line, lit, nOp, 0, "F()()", "mark location in source")
instruction(Local, lit, lcl, 0, "F()()", "introduce local variable")
