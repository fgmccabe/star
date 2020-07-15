//
// Created by Francis McCabe on 7/10/20.
//

#ifndef STAR_X86_64P_H
#define STAR_X86_64P_H
#include "x86_64.h"

void emitU8(x64CtxPo ctx, u8 byte);
void emitU16(x64CtxPo ctx, u16 word);
void emitU32(x64CtxPo ctx, u32 word);
void emitU64(x64CtxPo ctx, u64 word);


#define REX_W    0x48
#define REX_R    0x44
#define REX_X    0x42
#define REX_B    0x41
#define REX    0x40

/* Size flags for emit_x86_instruction: */
#define EX86_BIN_INS    0x0010
#define EX86_SHIFT_INS    0x0020
#define EX86_REX    0x0040
#define EX86_NO_REXW    0x0080
#define EX86_BYTE_ARG    0x0100
#define EX86_HALF_ARG    0x0200
#define EX86_PREF_66    0x0400
#define EX86_PREF_F2    0x0800
#define EX86_PREF_F3    0x1000
#define EX86_SSE2_OP1    0x2000
#define EX86_SSE2_OP2    0x4000
#define EX86_SSE2    (EX86_SSE2_OP1 | EX86_SSE2_OP2)

#define OPERAND_PREFIX  0x66
#define ADDRESS_PREFIX  0x67

// Status flags masks
#define CF              (0u)
#define PF              (1u<<2u)
#define AF              (1u<<4u)
#define ZF              (1u<<6u)
#define SF              (1u<<7u)
#define DF              (1u<<10u)
#define OF              (1u<<11u)

#define ADD    (/* BINARY */ 0 << 3)
#define ADD_EAX_i32  0x05
#define ADD_r_rm  0x03
#define ADD_rm_r  0x01
#define ADDSD_x_xm  0x58
#define ADC    (/* BINARY */ 2 << 3)
#define ADC_EAX_i32  0x15
#define ADC_r_rm  0x13
#define ADC_rm_r  0x11
#define AND    (/* BINARY */ 4 << 3)
#define AND_EAX_i32  0x25
#define AND_r_rm  0x23
#define AND_rm_r  0x21
#define ANDPD_x_xm  0x54
#define BSR_r_rm  (/* GROUP_0F */ 0xbd)
#define CALL_i32  0xe8
#define CALL_rm    (/* GROUP_FF */ 2 << 3)
#define CDQ    0x99
#define CMOVE_r_rm  (/* GROUP_0F */ 0x44)
#define CMP    (/* BINARY */ 7 << 3)
#define CMP_EAX_i32  0x3d
#define CMP_r_rm  0x3b
#define CMP_rm_r  0x39
#define CVTPD2PS_x_xm  0x5a
#define CVTSI2SD_x_rm  0x2a
#define CVTTSD2SI_r_xm  0x2c
#define DIV    (/* GROUP_F7 */ 6 << 3)
#define DIVSD_x_xm  0x5e
#define FSTPS    0xd9
#define FSTPD    0xdd
#define INT3    0xcc
#define IDIV    (/* GROUP_F7 */ 7 << 3)
#define IMUL    (/* GROUP_F7 */ 5 << 3)
#define IMUL_r_rm  (/* GROUP_0F */ 0xaf)
#define IMUL_r_rm_i8  0x6b
#define IMUL_r_rm_i32  0x69

#define JE              0x84
#define JNE             0x85
#define JC              0x82
#define JAE             0x83
#define JBE             0x86
#define JNBE            0x87
#define JL              0x8c
#define JNL             0x8d
#define JLE             0x8e
#define JNLE            0x8f
#define JO              0x80
#define JNO             0x81
#define JP              0x8a
#define JPO             0x8b

#define JMP_rm    (/* GROUP_FF */ 4 << 3)
#define LEA_r_m    0x8d
#define MOV_r_rm  0x8b
#define MOV_r_i32  0xb8u
#define MOV_rm_r  0x89
#define MOVSD_x_xm  0x10
#define MOVSD_xm_x  0x11
#define MOVSXD_r_rm  0x63
#define MOVSX_r_rm8  (/* GROUP_0F */ 0xbe)
#define MOVSX_r_rm16  (/* GROUP_0F */ 0xbf)
#define MOVZX_r_rm8  (/* GROUP_0F */ 0xb6)
#define MOVZX_r_rm16  (/* GROUP_0F */ 0xb7)
#define MUL    (/* GROUP_F7 */ 4 << 3)
#define MULSD_x_xm  0x59
#define NEG_rm    (/* GROUP_F7 */ 3 << 3)
#define NOP    0x90
#define NOT_rm    (/* GROUP_F7 */ 2 << 3)
#define OR    (/* BINARY */ 1 << 3)
#define OR_r_rm    0x0b
#define OR_EAX_i32  0x0d
#define OR_rm_r    0x09
#define OR_rm8_r8  0x08
#define POP_r    0x58
#define POP_rm    0x8f
#define POPF    0x9d
#define PREFETCH  0x18
#define PUSH_i32  0x68
#define PUSH_r    0x50
#define PUSH_rm    (/* GROUP_FF */ 6 << 3)
#define PUSHF    0x9c
#define RET_near  0xc3
#define RET_i16    0xc2
#define SBB    (/* BINARY */ 3 << 3)
#define SBB_EAX_i32  0x1d
#define SBB_r_rm  0x1b
#define SBB_rm_r  0x19
#define SAR    (/* SHIFT */ 7 << 3)
#define SHL    (/* SHIFT */ 4 << 3)
#define SHR    (/* SHIFT */ 5 << 3)
#define SUB    (/* BINARY */ 5 << 3)
#define SUB_EAX_i32  0x2d
#define SUB_r_rm  0x2b
#define SUB_rm_r  0x29
#define SUBSD_x_xm  0x5c
#define TEST_EAX_i32  0xa9
#define TEST_rm_r  0x85
#define UCOMISD_x_xm  0x2e
#define UNPCKLPD_x_xm  0x14
#define XCHG_EAX_r  0x90
#define XCHG_r_rm  0x87
#define XOR    (/* BINARY */ 6 << 3)
#define XOR_EAX_i32  0x35
#define XOR_r_rm  0x33
#define XOR_rm_r  0x31
#define XORPD_x_xm  0x57

#define GROUP_0F  0x0f
#define GROUP_F7  0xf7
#define GROUP_FF  0xff
#define GROUP_BINARY_81  0x81
#define GROUP_BINARY_83  0x83
#define GROUP_SHIFT_1  0xd1
#define GROUP_SHIFT_N  0xc1
#define GROUP_SHIFT_CL  0xd3

#define MOD_REG    0xc0
#define MOD_DISP8  0x40

#define HALFWORD_MAX 0x7fffffffl
#define HALFWORD_MIN -0x80000000l

#define IS_HALFWORD(x)    ((x) <= HALFWORD_MAX && (x) >= HALFWORD_MIN)
#define NOT_HALFWORD(x)    ((x) > HALFWORD_MAX || (x) < HALFWORD_MIN)

#endif //STAR_X86_64P_H
