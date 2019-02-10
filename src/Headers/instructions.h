/*
  Specification of the Star Bytecode Instruction set
  (c) 2017 F.G.McCabe

  This file should NEVER be included directly. It must always be
  preceded and followed by undefs. A typical sequence looks like:

#undef instruction
#define instruction(Op,A1,Dl,Cmnt) ...<expansion>...

#include "instruction.h"
#undef instruction

  Contact: Francis McCabe <frankmccabe@mac.com>
*/

instruction(Halt,nOp,0,"Stop execution")
instruction(Nop,nOp,0,"No operation")

instruction(Call,lit,0,"Call <prog>")
instruction(OCall,art,0,"OCall")
instruction(Escape,Es,1,"call C escape")
instruction(Tail,lit,0,"Tail <prog>")
instruction(OTail,art,0,"OTail")
instruction(Abort,tOs,0,"Abort execution")

instruction(Ret,tOs,-1,"return")

instruction(Jmp,off,0,"jump lbl")

instruction(Drop,tOs,-1,"drop top of stack")
instruction(Dup,tOs,1,"duplicate top of stack")
instruction(Pull,i32,1,"copy nth stack entry to top of stack")
instruction(Rot,i32,0,"rotate nth stack entry to top of stack")
instruction(Rst,i32,0,"reset stack height to a fixed height")

instruction(LdV,nOp,1,"Place a void value on stack")
instruction(LdG,glb,1,"load a global variable")
instruction(LdC,lit,1,"load literal from constant pool")
instruction(LdA,arg,1,"load stack from args[xx]")
instruction(LdL,lcl,1,"load stack from local[xx]")
instruction(StL,lcs,-1,"store tos to local[xx]")
instruction(TL,lcs,0,"copy tos to local[xx]")
instruction(StA,arg,-1,"store tos to args[xx]")
instruction(StG,glb,-1,"store into a global variable")

instruction(CLbl,off,-1,"T,Lbl --> test for a data term, branch if lbl")
instruction(CVd,nOp,-1,"Lbl --> test for void, branch if void")
instruction(Nth,i32,0,"T --> el, pick up the nth element")
instruction(StNth,i32,-2,"T el --> store in nth element")
instruction(Get,lit,0,"T --> el access a field from structure")
instruction(Set,lit,-1,"T el --> store in field of structure")

instruction(Case,i32,0,"T --> T, case <Max> ")

instruction(Alloc,lit,1,"new closure, code from constant pool")

instruction(Cmp,off,-1,"t1 t2 --> , branch to offset if not same literal")

instruction(Bf,off,-1,"bool --> branch if false")
instruction(Bt,off,-1,"bool --> branch if true")

instruction(Frame,i32,0,"frame instruction")

instruction(dLine,lne,0,"--> debug line")
instruction(dBug,nOp,0,"debugging prefix")
instruction(dBreak,nOp,0,"special instruction for break points")
