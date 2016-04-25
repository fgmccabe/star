/* 
 * Access the opcodes of the Star machine
 * (c) 2013 F.G.McCabe
 * all rights reserved
 **/

#ifndef _OPCODES_H_
#define _OPCODES_H_

/*
 * Assume a byte code architecture. This makes transitioning between 32bit and
 * 64 bit easier.
 * The general form of an instruction is:
 *
 * | Mnem | Op1 ... Opn |
 *
 * where each Opi is a big-ended sequence of bytes, depending on the precise
 * instruction.
 *
 * Special Registers include frame pointer
 *
 * TOS current top of expression stack
 * FP current frame pointer (access locals)
 * E current frame pointer (access free variables)
 * L current literals pointer
 * PC current program counter
 * PRG current program base
 */

typedef enum {
 nOp,                                   // No operand
 tos,         /* top of stack operand */
 i32,					/* 32 bit literal operand */
 arg,					/* argument variable offset */
 lcl,					/* local variable offset */
 env,					/* free variable offset */
 off,					/* offset within current code */
 Es,					// escape code 0..65535
 lit					/* constant literal */
} opAndSpec;                    // Specification code for an operand

#undef instruction
#define instruction(Op,A1,A2,Cmnt) Op, // Cmnt

typedef enum {
#include "instructions.h"	/* Pick up the instructions specification */
  DefineLbl,			/* Empty instruction to define a label */
  DefineFrame,			/* Frame instruction */
  illegalOp } OpCode;

#undef instruction

#endif

