//
// Created by Francis McCabe on 7/10/20.
//

#ifndef STAR_X86_64P_H
#define STAR_X86_64P_H


#include "x86_64.h"
#include "ooio.h"
#include "array.h"

typedef struct assem_lbl{
  char nm[128];
  arrayPo refs;
  integer pc;
} AssemLblRecord;

typedef struct lbl_ref{
  lblRefUpdater updater;
  integer pc;
} AssemLblRefRecord;

typedef struct assem_ctx {
  u8 *bytes;
  u32 size;
  u32 pc;
  hashPo lbls;
} AssemCtxRecord;

void emitU8(x64CtxPo ctx, u8 byte);
void emitU16(x64CtxPo ctx, u16 word);
void emitU32(x64CtxPo ctx, u32 word);
void emitU64(x64CtxPo ctx, u64 word);
void updateU32(x64CtxPo ctx, integer pc, u32 word);
u32 readCtxAtPc(x64CtxPo ctx, integer pc);

#define REX_W    0x48
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

#define ADD_r_rm  0x03
#define ADD_rm_r  0x01
#define ADD_rm_imm  0x81
#define ADC_r_rm  0x13
#define ADC_rm_r  0x11
#define ADC_rm_imm  0x81
#define AND_r_rm  0x23
#define AND_rm_r  0x21
#define AND_rm_imm  0x81
#define BSR_r_rm  (/* GROUP_0F */ 0xbd)
#define CALL_i32  0xe8
#define CALL_rm 0xff
#define CALL_lbl 0xe8
#define CDQ    0x99
#define CMOVE_r_rm  (/* GROUP_0F */ 0x44)

#define CMP_r_rm  0x3b
#define CMP_rm_r  0x39
#define CMP_rm_imm 0x81
#define CVTPD2PS_x_xm  0x5a
#define CVTSI2SD_x_rm  0x2a
#define CVTTSD2SI_r_xm  0x2c

#define DEC_rm 0xff
#define INC_rm 0xff

#define DIVSD_x_xm  0x5e
#define FSTPS    0xd9
#define FSTPD    0xdd

#define IDIV    0xf7

#define IMUL_r_rm_1  0x0f
#define IMUL_r_rm_2  0xaf
#define IMUL_r_rm_i32  0x69

#define JCC  0x0f
#define JMP_m 0xe9
#define JMP_rm 0xff
#define LEA_r_m    0x8d
#define MOV_r_rm  0x8b
#define MOV_r_i32  0xb8u
#define MOV_rm_r  0x89
#define MOV_rm_imm 0xc7
#define MUL    (/* GROUP_F7 */ 4 << 3)
#define MULSD_x_xm  0x59
#define NEG_rm 0xf7
#define NOP    0x90
#define NOT_rm  0xf7
#define OR    (/* BINARY */ 1 << 3)
#define OR_r_rm    0x0b
#define OR_rm_r    0x09
#define OR_rm_imm  0x81
#define POP_r    0x58
#define POP_rm    0x8f
#define POPF    0x9d
#define PREFETCH  0x18
#define PUSH_i32  0x68u
#define PUSH_r    0x50u
#define PUSH_rm   0xffu
#define PUSH_i8   0x6au
#define PUSH_i32  0x68u
#define PUSHF    0x9c
#define RET_near  0xc3
#define RET_i16    0xc2
#define SET_CC_rm  0x0f
#define SBB_r_rm  0x1b
#define SBB_rm_r  0x19
#define SBB_rm_imm 0x81
#define SAR    (/* SHIFT */ 7 << 3)
#define SHL    (/* SHIFT */ 4 << 3)
#define SHR    (/* SHIFT */ 5 << 3)

#define SUB_r_rm  0x2b
#define SUB_rm_r  0x29
#define SUB_rm_imm 0x81

#define SUBSD_x_xm  0x5c

#define TEST_RAX_imm  0xa9
#define TEST_rm_r  0x85
#define TEST_rm_imm  0xf7

#define UCOMISD_x_xm  0x2e
#define UNPCKLPD_x_xm  0x14
#define XCHG_EAX_r  0x90
#define XCHG_r_rm  0x87
#define XOR_r_rm  0x33
#define XOR_rm_r  0x31
#define XOR_rm_imm 0x81


#define MAX_I32 0x7fffffffl
#define MIN_I32 -0x80000000l

void initAssemX64();

logical isByte(i64 x);
logical isI32(i64 x);

void emitLblRef(x64CtxPo ctx, x64LblPo tgt);
void labelDisp32(x64CtxPo ctx, x64LblPo lbl, integer pc);

x64LblPo preamble(x64CtxPo ctx, i32 lclCount);
retCode postamble(x64CtxPo ctx);

#endif //STAR_X86_64P_H
