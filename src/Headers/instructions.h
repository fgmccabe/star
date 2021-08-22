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

instruction(Halt,i32,nOp,0,"Stop execution")
instruction(Abort,tOs,nOp,-2,"abort with message")

instruction(Call,sym,nOp,0,"Call <prog>")
instruction(OCall,art,nOp,0,"OCall")
instruction(Escape,Es,nOp,1,"call C escape")
instruction(TCall,sym,nOp,0,"TCall <prog>")
instruction(TOCall,art,nOp,0,"TOCall")

instruction(Ret,tOs,nOp,0,"return")

instruction(Jmp,off,nOp,0,"jump lbl")

instruction(Drop,tOs,nOp,-1,"drop top of stack")
instruction(DropTo,tOs,i32,0,"drop stack tp fixed level")
instruction(Dup,tOs,nOp,1,"duplicate top of stack")
instruction(Rst, i32, nOp, 0, "reset stack height to a fixed height")
instruction(Swap,tOs,nOp,0,"swap top two elements on stack")

instruction(Tag,sym,nOp,1,"allocate a marker")
instruction(Prompt,tOs,nOp,-2,"delimit a suspendable computation")
instruction(Cut,tOs,nOp,0,"find top prompt and cut stack there. Leave continuation on stack")
instruction(Resume,tOs,nOp,0,"restore a cut continuation with parameter")
instruction(Underflow,nOp,nOp,0,"underflow from current stack")

instruction(LdV,nOp,nOp,1,"Place a void value on stack")
instruction(LdG,glb,nOp,1,"load a global variable")
instruction(LdC,lit,nOp,1,"load literal from constant pool")
instruction(LdA,arg,nOp,1,"load stack from args[xx]")
instruction(LdL,lcl,nOp,1,"load stack from local[xx]")
instruction(StL,lcs,nOp,-1,"store tos to local[xx]")
instruction(StV,lcs,nOp,0,"clear a local to void")
instruction(TL,lcs,nOp,0,"copy tos to local[xx]")
instruction(StA,arg,nOp,-1,"store tos to args[xx]")
instruction(StG,glb,nOp,-1,"store into a global variable")
instruction(TG,glb,nOp,0,"copy into a global variable")

instruction(Cell,tOs,nOp,0,"create R/W cell")
instruction(Get,tOs,nOp,0,"access a R/W cell")
instruction(Assign,tOs,nOp,-1,"assign to a R/W cell")

instruction(CLbl,sym,off,0,"T,Lbl --> test for a data term, branch if lbl")
instruction(CmpVd,lVl,nOp,-1,"T --> test for void on the stack, break if void")
instruction(Nth, i32, nOp, 0, "T --> el, pick up the nth element")
instruction(StNth, i32, nOp, -2, "T el --> store in nth element")

instruction(If,off,nOp,-1,"break if true")
instruction(IfNot,off,nOp,-1,"brak if false")

instruction(Case, i32, nOp, 0, "T --> T, case <Max> ")
instruction(IndxJmp,i32,nOp,0,"check and jump on index")
instruction(Unpack,sym,off,-1,"check against term & unpack")

instruction(IAdd,tOs,nOp,-1,"L R --> L+R")
instruction(ISub,tOs,nOp,-1,"L R --> L-R")
instruction(IMul,tOs,nOp,-1,"L R --> L*R")
instruction(IDiv,tOs,nOp,-1,"L R --> L/R")
instruction(IMod,tOs,nOp,-1,"L R --> L%R")
instruction(IAbs,tOs,nOp,0,"L --> abs(L)")

instruction(IEq,tOs,nOp,-1,"L R --> L==R")
instruction(ILt,tOs,nOp,-1,"L R --> L<R")
instruction(IGe,tOs,nOp,-1,"L R --> L>=R")
instruction(ICmp,lVl,nOp,-2,"L R --> break if not same integer")

instruction(BAnd,tOs,nOp,-1,"L R --> L&R")
instruction(BOr,tOs,nOp,-1,"L R --> L|R")
instruction(BXor,tOs,nOp,-1,"L R --> L^R")
instruction(BLsl,tOs,nOp,-1,"L R --> L<<R")
instruction(BLsr,tOs,nOp,-1,"L R --> L>>R")
instruction(BAsr,tOs,nOp,-1,"L R --> L>>>R")
instruction(BNot,tOs,nOp,0,"L --> ~L")

instruction(FAdd,tOs,nOp,-1,"L R --> L+R")
instruction(FSub,tOs,nOp,-1,"L R --> L-R")
instruction(FMul,tOs,nOp,-1,"L R --> L*R")
instruction(FDiv,tOs,nOp,-1,"L R --> L/R")
instruction(FMod,tOs,nOp,-1,"L R --> L%R")
instruction(FAbs,tOs,nOp,0,"L --> abs(L)")

instruction(FEq,tOs,nOp,-2,"L R e --> L==R")
instruction(FLt,tOs,nOp,-1,"L R --> L<R")
instruction(FGe,tOs,nOp,1,"L R --> L>=R")
instruction(FCmp,off,nOp,-2,"L R --> branch if not same floating point")

instruction(Alloc,sym,nOp,1,"new structure, elements from stack")
instruction(AlTpl,sym,nOp,1,"new empty structure")

instruction(Cmp,off,nOp,-1,"t1 t2 --> , branch to offset if not same literal")
instruction(Comp,lVl,nOp,-1,"t1 t2 --> , break if not same literal")

instruction(Frame,tPe,nOp,0,"frame instruction")

instruction(dLine,lne,nOp,0,"--> debug line")
instruction(dBug,nOp,nOp,0,"debugging prefix")
instruction(dBreak,nOp,nOp,0,"special instruction for break points")

