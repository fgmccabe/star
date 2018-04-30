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
instruction(OCall,i32,"OCall")
instruction(Escape,Es,"call C escape")
instruction(Tail,lit,"Tail <prog>")
instruction(OTail,i32,"OTail")

instruction(Ret,tOs,"return")

instruction(Jmp,off,"jump lbl")

instruction(Drop,tOs,"drop top of stack")
instruction(Dup,tOs,"duplicate top of stack")
instruction(Pull,i32,"copy nth stack entry to top of stack")
instruction(Rot,i32,"rotate nth stack entry to top of stack")
instruction(Rst,i32,"reset stack height to a fixed height")

instruction(LdG,glb,"load a global variable")
instruction(LdC,lit,"load literal from constant pool")
instruction(LdA,arg,"load stack from args[xx]")
instruction(LdL,lcl,"load stack from local[xx]")
instruction(StL,lcs,"store tos to local[xx]")
instruction(TL,lcs,"copy tos to local[xx]")
instruction(StA,arg,"store tos to args[xx]")
instruction(StG,glb,"store into a global variable")


instruction(CLbl,off,"T,Lbl --> test for a data term, branch if not")
instruction(Nth,i32,"T --> el, pick up the nth element")
instruction(StNth,i32,"T el --> store in nth element")

instruction(Case,i32,"T --> T, case <Max> ")

instruction(Alloc,lit,"new closure, code from constant pool")

instruction(Cmp,off,"t1 t2 --> , branch to offset if not same literal")

instruction(Bf,off,"bool --> branch if false")
instruction(Bt,off,"bool --> branch if true")

instruction(Frame,i32,"frame instruction")
instruction(Line,lit," --> Source line number notification")
