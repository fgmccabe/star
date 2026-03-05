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

instruction(Halt, "Stop execution",  rEg)
instruction(Abort, "abort with message",  lit, rEg)

instruction(Call, "Call <prog>",  sym, rEgs)
instruction(OCall, "OCall",  art, rEg, rEgs)
instruction(Escape, "call C escape",  Es, rEgs)
instruction(XCall, "Call <prog>",  sym, rEgs)
instruction(XOCall, "OCall",  art, rEg, rEgs)
instruction(XEscape, "call C escape",  Es, rEgs)
instruction(TCall, "TCall <prog>",  sym, rEgs)
instruction(TOCall, "TOCall",  art, rEg, rEgs)
instruction(Entry, "locals definition",  i32)

instruction(Ret, "return",  rEg)
instruction(XRet, "return exception",  rEg)

instruction(Block, "block of instructions", bLk)
instruction(Valof, "return value from block", bLk)
instruction(Break, "leave block", lVl)
instruction(Result, "return value out of block",  rEg, lVl)
instruction(Loop, "jump back to start of block", lVl)

instruction(Fiber, "Create new fiber",  rEg, rEg)
instruction(Suspend, "suspend fiber",  rEg, rEg, rEg)
instruction(Resume, "resume fiber",  rEg, rEg, rEg)
instruction(Retire, "retire a fiber",  rEg, rEg)
instruction(Underflow, "underflow from current stack")

instruction(LdC, "load literal from constant pool", rEg, lit)
instruction(Mv, "move from one reg to another",  rEg, rEg)

instruction(LdG, "load a global variable",  rEg, glb)
instruction(StG, "store into a global variable",  glb, rEg)

instruction(Bmp, "bump reference count", rEg)
instruction(Drp, "drop reference count", rEg)

instruction(Sav, "create a single assignment variable",  rEg)
instruction(LdSav, "derefence a sav, break if not set",  rEg, lVl)
instruction(TstSav, "test a sav, return a logical",  rEg, nOp)
instruction(StSav, "store a value into a single assignment variable",  rEg, rEg)

instruction(Cell, "create R/W cell",  rEg, rEg)
instruction(Get, "access a R/W cell",  rEg, rEg)
instruction(Assign, "assign to a R/W cell",  rEg, rEg)

instruction(CLbl, "T,Lbl --> test for a data term, break if not lbl",  sym, rEg, lVl)
instruction(CInt, "T,lit --> test for a literal integer, break if not",  lit, rEg, lVl)
instruction(CChar, "T,lit --> test for a literal char, break if not",  lit, rEg, lVl)
instruction(CFlt, "T,lit --> test for a literal floating point, break if not",  lit, rEg, lVl)
instruction(CLit, "T,lit --> test for a literal value, break if not",  lit, rEg, lVl)
instruction(Nth, "T --> el, pick up the nth element",  rEg, rEg, i32)
instruction(StNth, "T el --> store in nth element",  rEg, i32)

instruction(If, "break if true",  rEg, lVl)
instruction(IfNot, "break if false",  rEg, lVl)

instruction(ICase, "T --> T, icase <Max>",  i32, rEg)
instruction(Case, "T --> T, case <Max>",  i32, rEg)
instruction(IxCase, "check and jump on type index",  i32, rEg)

instruction(IAdd, "L R --> L+R",  rEg, rEg, rEg)
instruction(ISub, "L R --> L-R",  rEg, rEg, rEg)
instruction(IMul, "L R --> L*R",  rEg, rEg, rEg)
instruction(IDiv, "L R --> L/R",  rEg, rEg, rEg, lVl)
instruction(IMod, "L R --> L%R",  rEg, rEg, rEg, lVl)
instruction(IAbs, "L --> abs(L)",  rEg, rEg)

instruction(IEq, "L R --> L==R",  rEg, rEg, rEg)
instruction(ILt, "L R --> L<R",  rEg, rEg, rEg)
instruction(IGe, "L R --> L>=R",  rEg, rEg, rEg)

instruction(CEq, "L R --> L==R",  rEg, rEg, rEg)
instruction(CLt, "L R --> L<R",  rEg, rEg, rEg)
instruction(CGe, "L R --> L>=R",  rEg, rEg, rEg)

instruction(BAnd, "L R --> L&R",  rEg, rEg, rEg)
instruction(BOr, "L R --> L|R",  rEg, rEg, rEg)
instruction(BXor, "L R --> L^R",  rEg, rEg, rEg)
instruction(BLsl, "L R --> L<<R",  rEg, rEg, rEg)
instruction(BLsr, "L R --> L>>R",  rEg, rEg, rEg)
instruction(BAsr, "L R --> L>>>R",  rEg, rEg, rEg)
instruction(BNot, "L --> ~L",  rEg, rEg)

instruction(FAdd, "L R --> L+R",  rEg, rEg, rEg)
instruction(FSub, "L R --> L-R",  rEg, rEg, rEg)
instruction(FMul, "L R --> L*R",  rEg, rEg, rEg)
instruction(FDiv, "L R --> L/R",  rEg, rEg, rEg, lVl)
instruction(FMod, "L R --> L%R",  rEg, rEg, rEg, lVl)
instruction(FAbs, "L --> abs(L)",  rEg, rEg)

instruction(FEq, "L R e --> L==R",  rEg, rEg, rEg)
instruction(FLt, "L R --> L<R",  rEg, rEg, rEg)
instruction(FGe, "L R --> L>=R",  rEg, rEg, rEg)

instruction(Alloc, "new structure, elements from stack",  rEg, sym, rEgs)
instruction(Closure, "allocate a closure",  rEg, sym, rEg)

instruction(Frame, "frame instruction",  i32m lit)

instruction(Line, "source line indicator",  lit)
instruction(Bind, "bind a variable",  lit, rEg)
instruction(dBug, "debugging prefix",  lit)
