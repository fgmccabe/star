/*
  Specification of the Star Bytecode Instruction set
  (c) 2013 F.G.McCabe

  This file should NEVER be included directly. It must always be
  preceded and followed by undefs. A typical sequence looks like:

#undef instruction
#define instruction(Op,A1,A2,Cmnt) ...<expansion>...

#include "instruction.h"
#undef instruction

  Contact: Francis McCabe <frankmccabe@mac.com>
*/

instruction(Halt,tos,halt,"Stop execution")

instruction(Call,nOp,call,"Call ")
instruction(Escape,Es,escape,"call C escape")
instruction(Tail,nOp,tail,"Tail")

instruction(Enter,i32,enter,"enter <envsize>")

instruction(Ret,nOp,ret,"return")

instruction(Jmp,off,jmp,"jump lbl")

instruction(Pop,nOp,pop,"pop top of stack")
instruction(Dup,nOp,dup,"duplicate top of stack")
instruction(Pull,i32,pull,"pull nth stack entry to top of stack")
instruction(Swap,nOp,swap,"swap top 2 elements of stack")

instruction(LdInt,i32,ld,"load small integer literal ")
instruction(LdConst,lit,ld,"load literal from constant pool")
instruction(LdArg,arg,ld,"load stack from args[xx]")
instruction(LdLocal,lcl,ld,"load stack from local[xx]")
instruction(LdEnv,env,ld,"load stack from env[xx]")
instruction(StLocal,lcl,st,"store tos to local[xx]")
instruction(StArg,env,st,"store tos to args[xx]")
instruction(StEnv,env,st,"store tos to env[xx]")

instruction(Nth,i32,nth,"pick up the nth element")
instruction(StNth,i32,stnth,"store in nth element")

instruction(Cayse,i32,case,"case <Max>")

instruction(Alloc,lit,alloc,"new closure, code from constant pool")

instruction(I2f,nOp,i2f,"int --> float")
instruction(F2i,nOp,f2i,"float --> int")

instruction(Add,off,add,"int int --> int, branch to off on overflow")
instruction(Addf,off,addf,"float float --> float, branch to off on overflow")

instruction(Inc,off,inc,"int --> int+1, branch to off on overflow ")

instruction(Sub,off,sub,"x y --> x-y, branch to off on overflow")
instruction(Subf,off,subf,"x y --> x-y, branch to off on overflow")

instruction(Dec,off,dec,"x --> x-1, branch to off on overflow")

instruction(Mul,off,mul,"x y --> x*y, branch to off on overflow")
instruction(Mulf,off,mulf,"x y --> x*y, branch to off on overflow")

instruction(Div,off,div,"x y --> x//y, branch to off on overflow")
instruction(Divf,off,divf,"x y --> x/y, branch to off on overflow")

instruction(Rem,off,rem,"x y --> x%y, branch to off on overflow")

instruction(Left,nOp,left,"x y --> xx*2^yy + flags shift left")

instruction(Asr,nOp,asr,"xx yy --> xx/2^yy + flags shift arithmetic right")
instruction(Right,nOp,right,"xx yy --> xx/2^yy + flags shift arithmetic right")

instruction(Cmp,nOp,cmp,"x y --> res, result of x-y <,0,> ")
instruction(Cmpf,nOp,cmpf,"x y --> compare float, <,0,>")

instruction(Bz,off,bz,"x --> branch if zero")
instruction(Bnz,off,bnz,"x --> branch if non-zero")
instruction(Blt,off,blt,"x --> branch if less than zero")
instruction(Ble,off,ble,"x --> branch if less or equal to zero")
instruction(Bge,off,bge,"x --> branch if greater or equal to zero")
instruction(Bgt,off,bgt,"x --> branch if greater than zero")

instruction(Cas,off,cas,"... x y z --> ... ( x := z if [x]=y, else branch)")
