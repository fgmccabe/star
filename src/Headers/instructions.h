/*
  Specification of the Star Bytecode Instruction set
  (c) 2017, and beyond F.G.McCabe

  This file should NEVER be included directly. It must always be
  preceded and followed by undefs. A typical sequence looks like:

#undef instruction
#define instruction(Op,A1,Dl,Cmnt) ...<expansion>...

#include "instruction.h"
#undef instruction

  Contact: Francis McCabe <fmccabe@gmail.com>
*/

instruction(Halt,tOs,0,"Stop execution")

instruction(Call,sym,0,"Call <prog>")
instruction(OCall,art,0,"OCall")
instruction(Escape,Es,1,"call C escape")
instruction(Tail,sym,0,"Tail <prog>")
instruction(OTail,art,0,"OTail")

instruction(Ret,tOs,0,"return")

instruction(Jmp,off,0,"jump lbl")

instruction(Drop,tOs,-1,"drop top of stack")
instruction(Dup,tOs,1,"duplicate top of stack")
instruction(Rst,i32,0,"reset stack height to a fixed height")

instruction(LdV,nOp,1,"Place a void value on stack")
instruction(LdG,glb,1,"load a global variable")
instruction(LdC,lit,1,"load literal from constant pool")
instruction(LdA,arg,1,"load stack from args[xx]")
instruction(LdL,lcl,1,"load stack from local[xx]")
instruction(StL,lcs,-1,"store tos to local[xx]")
instruction(StV,lcs,0,"clear a local to void")
instruction(TL,lcs,0,"copy tos to local[xx]")
instruction(StA,arg,-1,"store tos to args[xx]")
instruction(StG,glb,-1,"store into a global variable")
instruction(TG,glb,0,"copy into a global variable")

instruction(CLbl,off,-1,"T,Lbl --> test for a data term, branch if lbl")
instruction(Nth,i32,0,"T --> el, pick up the nth element")
instruction(StNth,i32,-2,"T el --> store in nth element")
instruction(Get,sym,0,"T --> el access a field from structure")
instruction(Set,sym,-1,"T el --> store in field of structure")

instruction(Case,i32,0,"T --> T, case <Max> ")

instruction(IAdd,tOs,-1,"L R --> L+R")
instruction(ISub,tOs,-1,"L R --> L-R")
instruction(IMul,tOs,-1,"L R --> L*R")
instruction(IDiv,tOs,-1,"L R --> L/R")
instruction(IMod,tOs,-1,"L R --> L%R")
instruction(IAbs,tOs,0,"L --> abs(L)")

instruction(IEq,tOs,-1,"L R --> L==R")
instruction(ILt,tOs,-1,"L R --> L<R")
instruction(IGe,tOs,-1,"L R --> L>=R")

instruction(BAnd,tOs,-1,"L R --> L&R")
instruction(BOr,tOs,-1,"L R --> L|R")
instruction(BXor,tOs,-1,"L R --> L^R")
instruction(BLsl,tOs,-1,"L R --> L<<R")
instruction(BLsr,tOs,-1,"L R --> L>>R")
instruction(BAsr,tOs,-1,"L R --> L>>>R")
instruction(BNot,tOs,0,"L --> ~L")

instruction(FAdd,tOs,-1,"L R --> L+R")
instruction(FSub,tOs,-1,"L R --> L-R")
instruction(FMul,tOs,-1,"L R --> L*R")
instruction(FDiv,tOs,-1,"L R --> L/R")
instruction(FMod,tOs,-1,"L R --> L%R")
instruction(FAbs,tOs,0,"L --> abs(L)")

instruction(FEq,tOs,-2,"L R e --> L==R")
instruction(FLt,tOs,-1,"L R --> L<R")
instruction(FGe,tOs,-1,"L R --> L>=R")

instruction(Alloc,sym,1,"new structure, code from constant pool")

instruction(Cmp,off,-1,"t1 t2 --> , branch to offset if not same literal")

instruction(Bf,off,-1,"bool --> branch if false")
instruction(Bt,off,-1,"bool --> branch if true")

instruction(Frame,i32,0,"frame instruction")

instruction(Throw,off,0,"T --> T throw to handler or out")
instruction(Unwind,off,0,"jump to handler")

instruction(dLine,lne,0,"--> debug line")
instruction(dBug,nOp,0,"debugging prefix")
instruction(dBreak,nOp,0,"special instruction for break points")
