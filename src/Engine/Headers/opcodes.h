/* 
 * Access the opcodes of the Star machine
 * (c) 2013, 2018 and beyond F.G.McCabe
 * all rights reserved
 **/

#ifndef _OPCODES_H_
#define _OPCODES_H_

#undef instruction
#define instruction(Op, A,B, C, Cmt) Op,

typedef enum {
#include "instructions.h"

#undef instruction
  label,
  illegalOp,
  maxOpCode
} OpCode;

typedef enum {
  nOp,                                   // No operand
  tOs,          // top of stack
  i32,         /* 32 bit literal operand */
  art,          /* Arity */
  arg,          /* argument variable offset */
  lcl,          /* local variable offset */
  lcs,          // Store to local variable
  off,          /* offset within current code */
  Es,           // escape code 0..65535
  lit,          /* constant literal */
  sym,          // Symbol
  glb,          // Global variable name
  tPe,          // Type signature
  bLk,          // A block of instructions
  lVl,          // How many blocks to break out of
} opAndSpec;                    // Specification code for an operand

#endif

