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

instruction(Call,nOp,"Call ")
instruction(Escape,Es,"call C escape")
instruction(Tail,nOp,"Tail")

instruction(Enter,i32,"enter <envsize>")

instruction(Ret,i32,"return")

instruction(Jmp,off,"jump lbl")

instruction(Drop,nOp,"drop top of stack")
instruction(Dup,nOp,"duplicate top of stack")
instruction(Pull,i32,"copy nth stack entry to top of stack")
instruction(Rot,i32,"rotate nth stack entry to top of stack")

instruction(LdI,i32,"load small integer literal")
instruction(LdC,lit,"load literal from constant pool")
instruction(LdA,arg,"load stack from args[xx]")
instruction(LdL,lcl,"load stack from local[xx]")
instruction(LdE,env,"load stack from env[xx]")
instruction(StL,lcl,"store tos to local[xx]")
instruction(StA,env,"store tos to args[xx]")
instruction(StE,env,"store tos to env[xx]")

instruction(Nth,i32,"pick up the nth element")
instruction(StNth,i32,"store in nth element")

instruction(Case,i32,"case <Max>")

instruction(Alloc,lit,"new closure, code from constant pool")

instruction(I2f,nOp,"int --> float")
instruction(F2i,nOp,"float --> int")

instruction(AddI,nOp,"int int --> int")
instruction(AddF,nOp,"float float --> float")
instruction(LAdd,nOp,"int flags int --> flags int")

instruction(IncI,nOp,"int --> int+1")

instruction(SubI,nOp,"x y --> x-y")
instruction(SubF,nOp,"x y --> x-y")
instruction(LSub,nOp,"int flags int --> flags int")

instruction(DecI,nOp,"x --> x-1")

instruction(MulI,nOp,"x y --> x*y")
instruction(MulF,nOp,"x y --> x*y")
instruction(LMul,nOp,"int flags int --> flags int")

instruction(DivI,nOp,"x y --> x//y")
instruction(DivF,nOp,"x y --> x/y")
instruction(LDiv,nOp,"int flags int --> flags int")

instruction(RemI,nOp,"x y --> x%y")
instruction(RemF,nOp,"x y --> x%y")
instruction(LRem,nOp,"int flags int --> flags int")

instruction(Lft,nOp,"x y --> flags xx*2^yy")
instruction(LLft,nOp,"x flags y --> flags xx*2^yy")

instruction(Asr,nOp,"x y --> flags x/2^y shift arithmetic right")
instruction(Rgt,nOp,"x y --> flags x/2^y shift arithmetic right")

instruction(CmpI,nOp,"x y --> flags, result of x-y <,0,> ")
instruction(LCmp,nOp,"x flags y --> flags, result of x-y <,0,> ")

instruction(CmpF,nOp,"x y --> flags, compare float, <,0,>")

instruction(Bz,off,"flags --> branch if zero")
instruction(Bnz,off,"flags --> branch if non-zero")
instruction(Bf,off,"flags --> branch if overflow")
instruction(Bnf,off,"flags --> branch if nont overflow")
instruction(Blt,off,"flags --> branch if less than zero")
instruction(Ble,off,"flags --> branch if less or equal to zero")
instruction(Bge,off,"flags --> branch if greater or equal to zero")
instruction(Bgt,off,"flags --> branch if greater than zero")

instruction(Cas,off,"... x y z --> ... ( x := z if [x]=y, else branch)")
