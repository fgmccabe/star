/* 
 * Access the opcodes of the Star machine
 * (c) 2013, 2018 and beyond F.G.McCabe
 * all rights reserved
 **/

#ifndef _OPCODES_H_
#define _OPCODES_H_

#undef instruction
#define instruction(Op, Nd, Dl, Cmt) Op,

typedef enum {
#include "instructions.h"

#undef instruction
  label,
  illegalOp
} OpCode;

/*
 * Assume a byte code architecture. This makes transitioning between 32bit and
 * 64 bit easier.
 * The general form of an instruction is:
 *
 * | Mnem | Op |
 *
 * where each Op is a big-ended sequence of bytes, depending on the precise
 * instruction.
 *
 * Special Registers include frame pointer
 *
 * TOS current top of expression stack
 * FP current frame pointer (access locals)
 * L current literals pointer
 * PC current program counter
 * PRG current program base
 */

typedef enum {
  nOp,                                   // No operand
  tOs,
  i32,          /* 32 bit literal operand */
  art,          /* Arity */
  arg,          /* argument variable offset */
  lcl,          /* local variable offset */
  lcs,        // Store to local variable
  off,          /* offset within current code */
  Es,          // escape code 0..65535
  lit,          /* constant literal */
  lne,          // Constant that is a location
  glb           // Global variable name
} opAndSpec;                    // Specification code for an operand
#endif

