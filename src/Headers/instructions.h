/*
  Specification of the Star Bytecode Instruction set
  (c) 2017 F.G.McCabe

  This file should NEVER be included directly. It must always be
  preceded and followed by undefs. A typical sequence looks like:

#undef instruction
#define instruction(Op,A1,Cmnt) ...<expansion>...

#include "instruction.h"
#undef instruction

  Contact: Francis McCabe <frankmccabe@mac.com>
*/

instruction(Halt,nOp,"Stop execution")

instruction(Call,lit,"Call <prog>")
instruction(OCall,nOp,"OCall")
instruction(Escape,Es,"call C escape")
instruction(Tail,lit,"Tail <prog>")
instruction(OTail,nOp,"OTail")

instruction(Enter,i32,"enter <envsize>")

instruction(Ret,nOp,"return")

instruction(Jmp,off,"jump lbl")

instruction(Drop,nOp,"drop top of stack")
instruction(Dup,nOp,"duplicate top of stack")
instruction(Pull,i32,"copy nth stack entry to top of stack")
instruction(Rot,i32,"rotate nth stack entry to top of stack")

instruction(LdC,lit,"load literal from constant pool")
instruction(LdA,arg,"load stack from args[xx]")
instruction(LdL,lcl,"load stack from local[xx]")
instruction(StL,lcl,"store tos to local[xx]")
instruction(TL,lcl,"copy tos to local[xx]")
instruction(StA,arg,"store tos to args[xx]")

instruction(Nth,i32,"pick up the nth element")
instruction(StNth,i32,"store in nth element")

instruction(Hash,nOp,"x --> hash(x), access hash of tos")
instruction(Case,i32,"hash --> , case <Max> ")

instruction(Alloc,lit,"new closure, code from constant pool")

instruction(Bf,off,"bool --> branch if false")
instruction(Bt,off,"bool --> branch if true")

instruction(Cas,off,"... x y z --> ... ( x := z if [x]=y, else branch)")

instruction(Frame,lit,"frame instruction")
